package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val w0x = 1 - point.x
    val w0y = 1 - point.y
    val w1x = point.x
    val w1y = point.y

    val d0x = d00 * w0x + d10 * w1x
    val d1x = d01 * w0x + d11 * w1x
    
    d0x * w0y + d1x * w1y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    import Interaction.{getPixelTiles, tileLocation, Alpha, RealTileSize}
    import Visualization.interpolateColor

    def interpolateTemp(loc: Location): Temperature = {
      val g00 = GridLocation(floor(loc.lat).toInt, floor(loc.lon).toInt)
      val g10 = GridLocation(ceil(loc.lat).toInt, floor(loc.lon).toInt)
      val g01 = GridLocation(floor(loc.lat).toInt, ceil(loc.lon).toInt)
      val g11 = GridLocation(ceil(loc.lat).toInt, ceil(loc.lon).toInt)
      val point = CellPoint(loc.lat - g00.lat, loc.lon - g00.lon)
      bilinearInterpolation(point, grid(g00), grid(g01), grid(g10), grid(g11))
    }

    val pixelTiles = getPixelTiles(tile)
    val pixelArray = pixelTiles.map{ tile =>
      val location = tileLocation(tile)
      val temp = interpolateTemp(location)
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, Alpha)
    }
    Image(RealTileSize, RealTileSize, pixelArray.toArray)
  }
}
