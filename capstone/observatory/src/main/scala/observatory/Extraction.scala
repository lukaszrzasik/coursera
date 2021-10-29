package observatory

import java.time.LocalDate
import java.nio.file.{Files, Path}

import scala.io.Source
import scala.math.rint

import org.apache.log4j.{Level, Logger}

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.types._
  import org.apache.spark.sql._
  import org.apache.spark.rdd._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Extraction")
      .master("local")
      .getOrCreate()

  import spark.implicits._

  def writeFileToTemp(file: String): Path = {
    val tempFile = Files.createTempFile(null, null)
    val buffer = Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
    Files.write(tempFile, buffer.toArray.map(_.toByte))
    tempFile
  }

  def convertFarenheitToCelsius(far: Double): Double = rint((far - 32) * .5556)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
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
    val stations = spark.read.schema(stationsSchema).csv(stationsTempFile.toString)
    val temperaturesTempFile = writeFileToTemp(temperaturesFile)
    val temperatures = spark.read.schema(temperaturesSchema).csv(temperaturesTempFile.toString)
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
    sparkAverageRecords(spark.sparkContext.parallelize(records.toSeq)).collect().toSeq
  }

  def sparkAverageRecords(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    System.gc()
    val rdd = records
      .map{ case (date, location, temp) => (location, (temp, 1)) }
      .reduceByKey{ case ((temp1, counter1), (temp2, counter2)) => (temp1 + temp2, counter1 + counter2) }
      .map{ case (location, (sum, counter)) => (location, sum / counter) }
    rdd
  }

}
