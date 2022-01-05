package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._
import scala.collection.parallel.ParIterable

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val TileSize = 256
  val RealTileSize = 256
  val Alpha = 128
  val MaxZoom = 3

  var excTileCounter = 32400 //64800
  var excTileString = "lrzasik:\n"

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lon = tile.x / pow(2, tile.zoom) * 360 - 180
    val lat = atan(sinh(Pi * (1 - 2 * tile.y / pow(2, tile.zoom)))).toDegrees
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    excTileCounter -= 1
    if (excTileCounter < 100)
      excTileString += "tile " + excTileCounter + ", " + temperatures.toString + ", " + colors.toString + ", " + tile.toString + "\n"
    if (excTileCounter < 1) throw new Exception(excTileString)

    System.gc()
    tilePar(temperatures, colors, tile)
  }

  def tilePar(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    import Visualization._

//    println("lrzasik: starting getPixelTiles")
    val pixelTiles = getPixelTiles(tile) //.par
    val pixelArray = pixelTiles.map{ tile =>
//      println("lrzasik: starting tileLocation, tile:" + tile)
      val location = tileLocation(tile)
//      println("lrzasik: starting predictTemperature, tile:" + tile)
      val temp = predictTemperature(temperatures, location)
//      println("lrzasik: starting interpolateColor, tile:" + tile)
      val color = interpolateColor(colors, temp)
//      println("lrzasik: starting Pixel, tile:" + tile)
      Pixel(color.red, color.green, color.blue, Alpha)
    }
    if (TileSize != RealTileSize) {
      val upscaled = upscaleArray(pixelArray.toArray)
      Image(RealTileSize, RealTileSize, upscaled.toArray)
    }
    else Image(RealTileSize, RealTileSize, pixelArray.toArray)
  }

  def tileRDD(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    import Visualization._

    val pixelTiles = so.spark.sparkContext.parallelize(getPixelTiles(tile).toSeq)
    val pixelArray = pixelTiles.map{ tile =>
      val location = tileLocation(tile)
      val temp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, Alpha)
    }
    Image(TileSize, TileSize, pixelArray.collect)
  }

  def getPixelTiles(tile: Tile): Iterable[Tile] = {
    val pixelTiles = new Array[Tile](TileSize * TileSize)
    val initialX = tile.x
    val initialY = tile.y
    val newZoom =  (log(TileSize) / log(2) + tile.zoom).toInt
    for (idx <- (0 until pixelTiles.size) /*.par*/ ) {
      val newX = initialX * TileSize + idx % TileSize
      val newY = initialY * TileSize + idx / TileSize
      pixelTiles(idx) = Tile(newX, newY, newZoom)
    }
    pixelTiles
  }

  def upscaleArray(pixels: Array[Pixel]): Array[Pixel] = {
    val upscaled = new Array[Pixel](RealTileSize * RealTileSize)
    val multiplier = RealTileSize / TileSize
    for (idx <- (0 until upscaled.size) /*.par*/ ) {
      val uX = idx % RealTileSize
      val uY = idx / RealTileSize
      val oX = uX / multiplier
      val oY = uY / multiplier
      upscaled(idx) = pixels(oY * TileSize + oX)
    }
    upscaled
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData //.par
      zoom <- (0 to MaxZoom) //.par
      y <- (0 until pow(2, zoom).toInt) //.par
      x <- (0 until pow(2, zoom).toInt) //.par
    } generateImage(year, Tile(x, y, zoom), data)
  }

//  def main(args: Array[String]): Unit = {
//    test_tile
//  }
//
//  def test_tile(): Unit = {
//    val temps = List((Location(-90, -180), -100D), (Location(0, 0), 0D), (Location(90, 180), 100D))
//    val colors = List((-100D, Color(0, 0, 255)), (0D, Color(255,255,255)), (100D, Color(255,0,0)))
//    val inputTile = Tile(0,0,0)
//    val img = tile(temps, colors, inputTile)
//    img.output(new java.io.File("target/test-tile.png"))
//  }
}
