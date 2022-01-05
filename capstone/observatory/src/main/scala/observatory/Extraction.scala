package observatory

import java.time.LocalDate
import java.nio.file.{Files, Path}

import scala.io.Source
import scala.math.rint

// import org.apache.log4j.{Level, Logger}

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql._
import org.apache.spark.rdd._

package object so {
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Extraction")
      .master("local")
      .getOrCreate()
}


/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  
  var excCounterLoc = 1
  var excStringLoc = "lrzasik:\n"

  def writeFileToTemp(file: String): Path = {
    val tempFile = Files.createTempFile(null, null)
    val buffer = Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
    Files.write(tempFile, buffer.toArray.map(_.toByte))
    tempFile
  }

  def convertFarenheitToCelsius(far: Double): Double = rint((far - 32) * .5556)

  def toInt(str: String): Option[Int] = {
    try Some(str.toInt) catch { case _: Throwable => None }
  }

  def toDouble(str: String): Option[Double] = {
    try Some(str.toDouble) catch { case _: Throwable => None }
  }

  def parseStationsCSV(file: String): Iterator[(Option[Int], Option[Int], Option[Double], Option[Double])] = {
    val buffer = Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
    buffer.getLines.map{ line =>
      val list = line.split(",", -2)
      val stnid = toInt(list(0))
      val wbanid = toInt(list(1))
      val lat = toDouble(list(2))
      val lon = toDouble(list(3))
      (stnid, wbanid, lat, lon)
    }
  }

  def parseTemperaturesCSV(file: String): Iterator[(Option[Int], Option[Int], Option[Int], Option[Int], Option[Double])] = {
    val buffer = Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
    buffer.getLines.map{ line =>
      val list = line.split(",", -2)
      val stnid = toInt(list(0))
      val wbanid = toInt(list(1))
      val month = toInt(list(2))
      val day = toInt(list(3))
      val temp = toDouble(list(4))
      (stnid, wbanid, month, day, temp)
    }
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
//    locateTemperaturesDF(year, stationsFile, temperaturesFile)
    locateTemperaturesPar(year, stationsFile, temperaturesFile)
  }

  def locateTemperaturesPar(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = parseStationsCSV(stationsFile).toList
    val temperatures = parseTemperaturesCSV(temperaturesFile).toList

    locateTemperaturesIt(year, stations, temperatures)
  }

  def locateTemperaturesIt(year: Year, stations: List[(Option[Int], Option[Int], Option[Double], Option[Double])], temperatures: List[(Option[Int], Option[Int], Option[Int], Option[Int], Option[Double])]): Iterable[(LocalDate, Location, Temperature)] = {
//    excCounterLoc -= 1
//    if (excCounterLoc < 100) excStringLoc += "locateTemperatures " + excCounterLoc + "\n" + year.toString + "\n" + stations.toList.toString + "\n" + temperatures.toList.toString + "\n"
//    if (excCounterLoc < 1) throw new Exception(excStringLoc)

    val it = for {
      station <- stations if station._3 != None && station._4 != None
      temperature <- temperatures if station._1 == temperature._1 && station._2 == temperature._2
    } yield {
      (LocalDate.of(year, temperature._3.get, temperature._4.get), Location(station._3.get, station._4.get), convertFarenheitToCelsius(temperature._5.get))
    }
    it.toIterable
  }

  def locateTemperaturesDF(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
     import so.spark.implicits._

     val stationsSchema = StructType(
       StructField("STNid", IntegerType, false) ::
       StructField("WBANid", IntegerType, true) ::
       StructField("Lat", DoubleType, true) ::
       StructField("Lon", DoubleType, true) :: Nil
     )
     val temperaturesSchema = StructType(
       StructField("STNid", IntegerType, false) ::
       StructField("WBANid", IntegerType, true) ::
       StructField("Month", IntegerType, true) ::
       StructField("Day", IntegerType, true) ::
       StructField("Temperature", DoubleType, true) :: Nil
     )
     val stationsTempFile = writeFileToTemp(stationsFile)
     val stations = so.spark.read.schema(stationsSchema).csv(stationsTempFile.toString)
     val temperaturesTempFile = writeFileToTemp(temperaturesFile)
     val temperatures = so.spark.read.schema(temperaturesSchema).csv(temperaturesTempFile.toString)
     val result = stations
       .join(temperatures, stations("STNid") <=> temperatures("STNid") && stations("WBANid") <=> temperatures("WBANid"))
       .filter($"Lon".isNotNull && $"Lat".isNotNull)
       .select(stations("STNid"), stations("WBANid"), $"Lat", $"Lon", $"Month", $"Day", $"Temperature")
       .map{row: Row => ((year, row.getAs[Int]("Month"), row.getAs[Int]("Day")), Location(row.getAs("Lat"), row.getAs("Lon")), convertFarenheitToCelsius(row.getAs("Temperature")))}
       .collect
       .map{
         case ((year, month, day), location, temperature) => (LocalDate.of(year, month, day), location, temperature) }
     Files.delete(stationsTempFile)
     Files.delete(temperaturesTempFile)
     result
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
//    sparkAverageRecords(so.spark.sparkContext.parallelize(records.toSeq)).collect().toSeq
    locationYearlyAverageRecordsPar(records)
  }

  def locationYearlyAverageRecordsPar(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
   val par = records //.par
     .map{ case (date, location, temp) => (location, (temp, 1)) }
     .groupBy(_._1)
     .mapValues{ _.reduce[(Location, (Double, Int))]{ case ((loc1, (temp1, counter1)), (loc2, (temp2, counter2))) => (loc1, (temp1 + temp2, counter1 + counter2)) } }
     .mapValues{ case (location, (sum, counter)) => sum / counter }
   par.toList
  }

  def sparkAverageRecords(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    val rdd = records
      .map{ case (date, location, temp) => (location, (temp, 1)) }
      .reduceByKey{ case ((temp1, counter1), (temp2, counter2)) => (temp1 + temp2, counter1 + counter2) }
      .map{ case (location, (sum, counter)) => (location, sum / counter) }
    rdd
  }

}
