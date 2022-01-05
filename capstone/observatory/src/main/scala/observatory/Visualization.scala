package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._
import scala.collection.parallel.ParIterable

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val meanEarthRadius = 6371.009D
  val P = 1D

  var excCounterPredict = 129603 //129602 //129603 //129600 //64800 //129603 //129604
  var excStringPredict = "lrzasik:\n"

  var excCounterInterpolate = 504 //503 //500 //505 //515 //530 //560 //750 //500 //1011 //4050 //8100 //16200 //32400 //64800
  var excStringInterpolate = "lrzasik:\n"

  var excCounterVis = 101 //100 //101 //102 //105 //110 //115 //130 //100 //80 //40 //20 //131 //132 //133 //10
  var excStringVis = "lrzasik:\n"

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
//    if (excCounterPredict < 100)
//      excStringPredict += "predictTemperature " + excCounterPredict + ", " + temperatures.toString + ", " + location.toString + "\n"
//    excCounterPredict -= 1
//    if (excCounterPredict < 1) throw new Exception(excStringPredict)

    predictTemperaturePar(temperatures, location)
  }

  def predictTemperaturePar(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distancesPar = temperatures //.par
      .map{ temp => (greatCircleDistance(temp._1, location), temp._1, temp._2) }
    val exactMatchLocation = distancesPar.find{ _._1 == 0 }
    if (exactMatchLocation.isDefined) exactMatchLocation.get._3
    else {
      val distanceWeightsPar = distancesPar.map{ distance => (distanceWeight(distance._1), distance._2, distance._3) }
      distanceWeightsPar.aggregate(0D)(
        (acc: Double, weight: (Double, Location, Temperature)) => weight._1 * weight._3 + acc,
        (acc1: Double, acc2: Double) => acc1 + acc2) /
      distanceWeightsPar.aggregate(0D)(
        (acc: Double, weight: (Double, Location, Temperature)) => weight._1 + acc,
        (acc1: Double, acc2: Double) => acc1 + acc2)
    }
  }

  def predictTemperatureRDD(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val temperaturesRDD = so.spark.sparkContext.parallelize(temperatures.toSeq)
    val distancesRDD = temperaturesRDD.map{ temp => (greatCircleDistance(temp._1, location), temp._1, temp._2) }
    val exactMatchLocationRDD = distancesRDD.filter{ _._1 == 0 }
    if (!exactMatchLocationRDD.isEmpty) exactMatchLocationRDD.first._3
    else {
      val distanceWeightsRDD = distancesRDD.map{ distance => (distanceWeight(distance._1), distance._2, distance._3) }
      distanceWeightsRDD.aggregate(0D)(
        (acc: Double, weight: (Double, Location, Temperature)) => weight._1 * weight._3 + acc,
        (acc1: Double, acc2: Double) => acc1 + acc2) /
      distanceWeightsRDD.aggregate(0D)(
        (acc: Double, weight: (Double, Location, Temperature)) => weight._1 + acc,
        (acc1: Double, acc2: Double) => acc1 + acc2)
    }
  }

  def greatCircleDistance(loc1: Location, loc2: Location): Double = {
    val sigma = if (loc1 == loc2) 0
      else if (loc1.lon == loc2.lon - 180D && loc1.lat == -loc2.lat) Pi
      else if (loc1.lon == loc2.lon + 180D && loc1.lat == -loc2.lat) Pi
      else acos((sin(toRadians(loc1.lat))*sin(toRadians(loc2.lat))) + (cos(toRadians(loc1.lat))*cos(toRadians(loc2.lat))*cos(toRadians(abs(loc2.lon - loc1.lon)))))
    meanEarthRadius * sigma
  }

  def distanceWeight(distance: Double): Double = 1D / pow(distance, P)

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
//    excCounterInterpolate -= 1
//    if (excCounterInterpolate < 100)
//      excStringInterpolate += "interpolateColor " + excCounterInterpolate + ", " + points.toString + ", " + value + "\n"
//    if (excCounterInterpolate < 1) throw new Exception(excStringInterpolate)

    interpolateColorPar(points, value)
  }

  def interpolateColorPar(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val pointsPar = points //.par
    val point1 = pointsPar.minBy(item => abs(value - item._1))
    val point2 = pointsPar.filter(_ != point1).minBy(item => abs(value - item._1))
    interpolateColorPoints(point1, point2, value)
  }

  def interpolateColorRDD(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val pointsRDD = so.spark.sparkContext.parallelize(points.toSeq)
    val point1 = pointsRDD.min()(Ordering.by(i => abs(value - i._1)))
    val point2 = pointsRDD.filter(_ != point1).min()(Ordering.by(i => abs(value - i._1)))
    interpolateColorPoints(point1, point2, value)
  }

  def interpolateColorPoints(point1: (Temperature, Color), point2: (Temperature, Color), temp: Temperature): Color = {
    val red = convertToColorInt(interpolate(point1._1, point1._2.red, point2._1, point2._2.red, temp))
    val green = convertToColorInt(interpolate(point1._1, point1._2.green, point2._1, point2._2.green, temp))
    val blue = convertToColorInt(interpolate(point1._1, point1._2.blue, point2._1, point2._2.blue, temp))
    Color(red, green, blue)
  }

  def interpolate(x0: Double, y0: Int, x1: Double, y1: Int, x: Double): Int = {
    round((y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)).toInt
  }

  def convertToColorInt(input: Int): Int =
    if (input < 0) 0
    else if (input > 255) 255
    else input

  def tempDistance(temp1: Temperature, temp2: Temperature): Temperature = abs(temp1 - temp2)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
//    if (excCounterVis < 100)
//      excStringVis += "visualize " + excCounterVis + ", " + temperatures.toString + ", " + colors.toString + "\n"
//    excCounterVis -= 1
//    if (excCounterVis < 1) throw new Exception(excStringVis)

    System.gc()
    val w = 360
    val h = 180
    Image(w, h, convertToPixelArray(
      convertToPixelPosition(
      convertToLocationPixel(
      convertToLocationColor(temperatures, colors)), w, h), w, h))
  }

  def convertToLocationColor(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): /*Par*/ Iterable[(Location, Color)] = {
    temperatures //.par
      .map{ case (loc, temp) => (loc, interpolateColor(colors, temp)) }
  }

  def convertToLocationPixel(locations: /*Par*/ Iterable[(Location, Color)]): /*Par*/ Iterable[(Location, Pixel)] = {
    locations.map{ case (loc, color) => (loc, Pixel(color.red, color.green, color.blue, 255)) }
  }

  def convertToPixelPosition(pixels: /*Par*/ Iterable[(Location, Pixel)], width: Int, height: Int): /*Par*/ Iterable[((Int, Int), Pixel)] = {
    pixels.map{ case (loc, pixel) => ((width / 2 + round(loc.lon).toInt, height / 2 - round(loc.lat).toInt), pixel) }
  }

  def convertToPixelArray(pixels: /*Par*/ Iterable[((Int, Int), Pixel)], width: Int, height: Int): Array[Pixel] = {
    val arraySize = width * height
    val retPixels = Array.fill(arraySize)(Pixel(0, 0, 0, 0))
    for (pixel <- pixels) {
      var x = pixel._1._1
      x = if (x < width) x else width - 1
      var y = pixel._1._2
      y = if (y < height) y else height - 1
      val p = pixel._2
      val index = x + width * y
      retPixels(index) = p
    }
    retPixels
  }

}

