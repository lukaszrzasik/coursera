package observatory

import org.junit.Test
import org.junit.Assert._

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

  @Test def `test makeGrid`: Unit = {
    val szczecin = Location(53,14)
    val antipodeSzczecin = Location(-53,-166)
    val sydney = Location(-33,151)
    val betweenSzczecins = Location(0, -76)
    val betweenSzczecinSydney = Location(10, 82.5)
    val oneElementList = List((szczecin, 10D))
    val twoElementList = List((szczecin, 10D), (antipodeSzczecin, -10D))
    val twoElementList2 = List((szczecin, 10D), (sydney, 20D))
    val threeElementList = List((szczecin, 10D), (sydney, 20D), (antipodeSzczecin, -10D))

    val gridMaker = Manipulation.makeGrid(twoElementList)
    
    assertEquals(0D, gridMaker(GridLocation(0, 0)), 0.1D)
  }

}
