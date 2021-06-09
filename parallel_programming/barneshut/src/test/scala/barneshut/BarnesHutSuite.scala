package barneshut

import java.util.concurrent._
import scala.collection._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer
import org.junit._
import org.junit.Assert.{assertEquals, fail}

class BarnesHutSuite {
  // test cases for quad tree

import FloatOps._
  @Test def `Empty: center of mass should be the center of the cell`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  @Test def `Empty: mass should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  @Test def `Empty: total should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Leaf with 1 body`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  @Test def `Fork with 3 empty quadrants and 1 leaf (nw)`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  @Test def `Empty.insert(b) should return a Leaf with only that body (2pts)`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  @Test def `Body.updated should do nothing for Empty quad trees`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assertEquals(0f, body.xspeed, precisionThreshold)
    assertEquals(0f, body.yspeed, precisionThreshold)
  }

  @Test def `Body.updated should take bodies in a Leaf into account (2pts)`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  @Test def `'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)`: Unit = {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  @Test def `Fork.insert(b) should insert recursively in the appropriate quadrant`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 19.5f, 26.5f, 0f, 0f)
    val b3 = new Body(245f, 19.4f, 29f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1, b2, b3))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case fork: Fork => fork.nw match {
        case Fork(_, _, _, _) => {
        }
        case _ => fail("Leaf.insert() should have returned a Fork, was $newFork")
      }
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
  }

  @Test def `'insert' should work correctly on a leaf with center (1,1) and size 2`: Unit = {
    val b1 = new Body(245f, 0.4f, 0.4f, 0f, 0f)
    val quad = Leaf(1f, 1f, 2f, Seq(b1))
    val b = new Body(123f, 1.4f, 1.4f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case fork: Fork =>
        println(fork)
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
  }

  @Test def `Fork with 4 empty quadrants`: Unit = {
    val fork = new Fork(new Empty(5f, 5f, 10f), new Empty(15f, 5f, 10f), new Empty(5f, 15f, 10f), new Empty(15f, 15f, 10f))
    val b = new Body(10f, 2.5f, 2.5f, 0f, 0f)
    val inserter = fork.insert(b)
    assert(inserter.mass == 10f)
    assert(inserter.massX == 2.5f)
    assert(inserter.massY == 2.5f)
  }

  @Test def `basic test for SectorMatrix.+=`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 19.5f, 26.5f, 0f, 0f)
    val b3 = new Body(245f, 19.4f, 29f, 0f, 0f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += b1
    sm += b2
    sm += b3
  }

  @Test def `basic test for SectorMatrix.combine`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 19.5f, 26.5f, 0f, 0f)
    val b3 = new Body(245f, 19.4f, 29f, 0f, 0f)
    val b4 = new Body(123f, 1.4f, 1.4f, 0f, 0f)
    val b5 = new Body(123f, 50.4f, 24.8f, 0f, 0f)
    val b6 = new Body(123f, 24.8f, 50.4f, 0f, 0f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm3 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += b1
    sm1 += b2
    sm2 += b3
    sm2 += b4
    sm3 += b5
    sm3 += b6
    val sm = sm1.combine(sm2.combine(sm3))
    println(sm)
  }

  @Test def `basic test for computeSectorMatrix`: Unit = {
    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 19.5f, 26.5f, 0f, 0f)
    val b3 = new Body(245f, 19.4f, 29f, 0f, 0f)
    val bodies = Seq(b1, b2, b3)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = simulator.computeSectorMatrix(bodies, boundaries)
    println(sm)
  }

  @Test def `basic test for computeBoundaries`: Unit = {
    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 19.5f, 26.5f, 0f, 0f)
    val b3 = new Body(245f, 19.4f, 29f, 0f, 0f)
    val bodies = Seq(b1, b2, b3)
    val boundaries = simulator.computeBoundaries(bodies)
    println(boundaries)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

object FloatOps {
  val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

