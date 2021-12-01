package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  import Extraction._

  // Implement tests for the methods of the `Extraction` object

//  @Test def `test locateTemperature`: Unit = {
//    val result = locateTemperatures(2000, "/stations.csv", "/2000.csv")
//    println("locateTemperature result:")
//    result.take(20).foreach{ i => println(i.toString) }
//  }

  @Test def `test locateTemperaturesIt`: Unit = {
    var year = 2000
    var stations = List((Some(123),Some(456),Some(1.0),Some(-1.0)), (Some(123),None,Some(2.0),Some(-2.0)), (Some(456),None,Some(3.0),Some(-3.0)), (None,Some(123),Some(4.0),Some(-4.0)), (Some(456),Some(123),Some(5.0),Some(-5.0)))
    var temperatures = List((Some(123),Some(456),Some(1),Some(1),Some(50.0)), (Some(123),None,Some(1),Some(2),Some(50.0)), (Some(456),None,Some(1),Some(3),Some(50.0)), (None,Some(123),Some(1),Some(4),Some(50.0)), (Some(456),Some(123),Some(1),Some(5),Some(50.0)))
    var result = locateTemperaturesIt(year, stations, temperatures)
    println("result 1 =\n" + result.toList.toString)
  }

//  @Test def `test locationYearlyAverageRecords`: Unit = {
//    val result = locationYearlyAverageRecords(locateTemperatures(2000, "/stations.csv", "/2000.csv"))
//    println(result.take(20))
//  }

}
