package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  @Test def `test locateTemperature`: Unit = {
    val result = Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv")
    println(result.take(20))
  }

  @Test def `test locationYearlyAverageRecords`: Unit = {
    import Extraction._

    val result = Extraction.locationYearlyAverageRecords(locateTemperatures(2000, "/stations.csv", "/2000.csv"))
    println(result.take(20))
  }

}
