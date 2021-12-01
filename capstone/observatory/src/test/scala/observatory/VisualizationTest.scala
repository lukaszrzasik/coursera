package observatory

import org.junit.Assert._
import org.junit.Test
import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._
import scala.collection.parallel.immutable.ParVector

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  import Visualization._

//  @Test def `test greatCircleDistance`: Unit = {
//    val szczecin = Location(53,14)
//    val antipodeSzczecin = Location(-53, -166)
//    val sydney = Location(-33, 151)
//    assertEquals(0, greatCircleDistance(szczecin, szczecin), 0.1)
//    assertEquals(Pi * meanEarthRadius, greatCircleDistance(szczecin, antipodeSzczecin), 0.1)
//    assertEquals(15977D, greatCircleDistance(szczecin, sydney), 20)
//  }
//
//  @Test def `test distanceWeight`: Unit = {
//    assertEquals(1, distanceWeight(1), 0)
//    assertEquals(0.001, distanceWeight(1000), 0.0001)
//    assertEquals(1000, distanceWeight(0.001), 1)
//  }
//
//  @Test def `test predictTemperature`: Unit = {
//    val szczecin = Location(53,14)
//    val antipodeSzczecin = Location(-53,-166)
//    val sydney = Location(-33,151)
//    val betweenSzczecins = Location(0, -76)
//    val betweenSzczecinSydney = Location(10, 82.5)
//    val oneElementList = List((szczecin, 10D))
//    val twoElementList = List((szczecin, 10D), (antipodeSzczecin, -10D))
//    val twoElementList2 = List((szczecin, 10D), (sydney, 20D))
//    val threeElementList = List((szczecin, 10D), (sydney, 20D), (antipodeSzczecin, -10D))
//    assertEquals(10D, predictTemperature(oneElementList, szczecin), 0.1)
//    assertEquals(0D, predictTemperature(twoElementList, betweenSzczecins), 0.1)
//    assertEquals(15D, predictTemperature(twoElementList2, betweenSzczecinSydney), 1D)
//    assertEquals(10D, predictTemperature(threeElementList, szczecin), 0.1D)
//  }
//
//  @Test def `test tempDistance`: Unit = {
//    val temp1 = -2D
//    val temp2 = 2D
//    val temp3 = 0D
//    assertEquals(4D, tempDistance(temp1, temp2), 0D)
//    assertEquals(2D, tempDistance(temp1, temp3), 0D)
//    assertEquals(2D, tempDistance(temp2, temp3), 0D)
//  }
//
//  @Test def `test interpolate`: Unit ={
//    val point1 = (0D, 0)
//    val point2 = (10D, 0)
//    val point3 = (-10D, 0)
//    val point4 = (10D, 10)
//    val point5 = (10D, -10)
//    assertEquals(0, interpolate(point1._1, point1._2, point2._1, point2._2, 5D))
//    assertEquals(0, interpolate(point1._1, point1._2, point3._1, point3._2, -5D))
//    assertEquals(5, interpolate(point1._1, point1._2, point4._1, point4._2, 5D))
//    assertEquals(-5, interpolate(point1._1, point1._2, point5._1, point5._2, 5D))
//  }
//
//  @Test def `test interpolateColor for points`: Unit = {
//    val coldPoint = (-100D, Color(0, 0, 255))
//    val hotPoint = (100D, Color(255, 0, 0))
//    assertEquals(Color(128,0,128), interpolateColorPoints(coldPoint, hotPoint, 0D))
//  }
//
//  @Test def `test interpolateColor for iterable`: Unit = {
//    val coldPoint = (-100D, Color(0, 0, 255))
//    val hotPoint = (100D, Color(255, 0, 0))
//    val listFromTest = List((-6.103515625E-5,Color(255,0,0)), (0.0,Color(0,0,255)))
//    val valueFromTest = -10.00006103515625
//    assertEquals(Color(128,0,128), interpolateColor(coldPoint :: hotPoint :: Nil, 0D))
//    assertEquals(Color(255,0,0), interpolateColor(listFromTest, valueFromTest))
//  }
//
//  @Test def `basic color interpolation`: Unit = {
//    val firstPoint = (1D, Color(255, 0, 0))
//    val secondPoint = (2D, Color(0, 0, 255))
//    val thirdPoint = (3D, Color(0, 255, 0))
//    val list = firstPoint :: secondPoint :: thirdPoint :: Nil
//    val value1 = 1.5D
//    val value2 = 2.5D
//    assertEquals(Color(128,0,128), interpolateColor(list, value1))
//    assertEquals(Color(0,128,128), interpolateColor(list, value2))
//  }
//
//  @Test def `test convertToLocationColor`: Unit = {
//    val firstPoint = (1D, Color(255, 0, 0))
//    val secondPoint = (2D, Color(0, 0, 255))
//    val thirdPoint = (3D, Color(0, 255, 0))
//    val colorList = firstPoint :: secondPoint :: thirdPoint :: Nil
//    val value1 = 1.5D
//    val value2 = 2.5D
//    val value3 = 3.5D
//    val loc1 = Location(0, 0)
//    val loc2 = Location(90, 180)
//    val loc3 = Location(-90, -180)
//    val tempList = (loc1, value1) :: (loc2, value2) :: (loc3, value3) :: Nil
//    val expected = ParVector((Location(0.0,0.0),Color(128,0,128)), (Location(90.0,180.0),Color(0,128,128)), (Location(-90.0,-180.0),Color(0,255,0)))
//    assertEquals(expected, convertToLocationColor(tempList, colorList))
//  }
//
//  @Test def `test convertToLocationPixel`: Unit = {
//    val locations = ParVector((Location(0.0,0.0),Color(128,0,128)), (Location(90.0,180.0),Color(0,128,128)), (Location(-90.0,-180.0),Color(0,255,0)))
//    val expected = ParVector((Location(0.0,0.0),Pixel(-8388480)), (Location(90.0,180.0),Pixel(-16744320)), (Location(-90.0,-180.0),Pixel(-16711936)))
//    assertEquals(expected, convertToLocationPixel(locations))
//  }
//
//  @Test def `test convertToPixelPosition`: Unit = {
//    val pixels = ParVector((Location(0.0,0.0),Pixel(-8388480)), (Location(90.0,180.0),Pixel(-16744320)), (Location(-90.0,-180.0),Pixel(-16711936)))
//    val expected = ParVector(((180,90),Pixel(-8388480)), ((360,0),Pixel(-16744320)), ((0,180),Pixel(-16711936)))
//    assertEquals(expected, convertToPixelPosition(pixels, 360, 180))
//  }
//
//  @Test def `test convertToPixelArray`: Unit = {
//    val pixels = ParVector(((180,90),Pixel(-8388480)), ((360,0),Pixel(-16744320)), ((0,180),Pixel(-16711936)))
//    val expected = Array.fill(360 * 180)(Pixel(0, 0, 0, 0))
//    expected(180 + 90 * 360) = Pixel(-8388480)
//    expected(359 + 0 * 360) = Pixel(-16744320)
//    expected(0 + 179 * 360) = Pixel(-16711936)
//    val result = convertToPixelArray(pixels, 360, 180)
//    for (i <- 0 until expected.size)
//      assertEquals(expected(i), result(i))
//  }

}
