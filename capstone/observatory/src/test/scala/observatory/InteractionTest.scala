package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  import Interaction._

  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

//  @Test def `test tileLocation`: Unit = {
//    val maxY = 85.0511287798066
//    assertEquals(Location(maxY, -180), tileLocation(Tile(0, 0, 0)))
//    assertEquals(Location(maxY, 0D), tileLocation(Tile(1, 0, 1)))
//    assertEquals(Location(0D, -180), tileLocation(Tile(0, 1, 1)))
//    assertEquals(Location(0D, 0D), tileLocation(Tile(1, 1, 1)))
//  }
//
//  @Test def `test getPixelTiles`: Unit = {
//    val basicTile = Tile(0,0,0)
//    var expectedArray = new Array[Tile](TileSize * TileSize)
//    expectedArray(0) = Tile(0,0,8)
//    expectedArray(255) = Tile(255,0,8)
//    expectedArray(256) = Tile(0,1,8)
//    expectedArray(511) = Tile(255,1,8)
//    expectedArray(65535) = Tile(255,255,8)
//    expectedArray(65280) = Tile(0,255,8)
//    var resultArray = getPixelTiles(basicTile).toArray
//    assertEquals(expectedArray(0), resultArray(0))
//    assertEquals(expectedArray(255), resultArray(255))
//    assertEquals(expectedArray(256), resultArray(256))
//    assertEquals(expectedArray(511), resultArray(511))
//    assertEquals(expectedArray(65535), resultArray(65535))
//    assertEquals(expectedArray(65280), resultArray(65280))
//    val zoom1Tile1 = Tile(1,0,1)
//    expectedArray = new Array[Tile](TileSize * TileSize)
//    expectedArray(0) = Tile(256,0,9)
//    expectedArray(255) = Tile(511,0,9)
//    expectedArray(256) = Tile(256,1,9)
//    expectedArray(511) = Tile(511,1,9)
//    expectedArray(65535) = Tile(511,255,9)
//    expectedArray(65280) = Tile(256,255,9)
//    resultArray = getPixelTiles(zoom1Tile1).toArray
//    assertEquals(expectedArray(0), resultArray(0))
//    assertEquals(expectedArray(255), resultArray(255))
//    assertEquals(expectedArray(256), resultArray(256))
//    assertEquals(expectedArray(511), resultArray(511))
//    assertEquals(expectedArray(65535), resultArray(65535))
//    assertEquals(expectedArray(65280), resultArray(65280))
//  }
//
//  @Test def `test tile`: Unit = {
//    val temps = WrappedArray((Location(45.0,-90.0),20.0), (Location(45.0,90.0),0.0), (Location(0.0,0.0),10.0), (Location(-45.0,-90.0),0.0), (Location(-45.0,90.0),20.0))
//    val colors = List((0.0,Color(255,0,0)), (10.0,Color(0,255,0)), (20.0,Color(0,0,255)))
//    val inputTile = Tile(1,1,1)
//    val img = tile(temps, colors, inputTile)
//    img.output(new java.io.File("target/test-tile.png"))
//  }
}
