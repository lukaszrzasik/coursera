package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case exc: Throwable => { println(exc); false }
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  @Test def `test groupedPostings`: Unit = {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    println("grouped:")
    grouped.collect.foreach(println(_))
  }

  @Test def `test scoredPostings`: Unit = {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored = testObject.scoredPostings(grouped)
    assert(scored.filter(_._1.id == 6).collect.length > 0)
    assert(scored.filter(_._1.id == 42).collect.length > 0)
    assert(scored.filter(_._1.id == 72).collect.length > 0)
    assert(scored.filter(_._1.id == 126).collect.length > 0)
    assert(scored.filter(_._1.id == 174).collect.length > 0)
    assert(scored.collect.length == 2121822)
    println("scored:")
    scored.collect.foreach(println(_))
  }

  @Test def `test vectorPostings`: Unit = {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored = testObject.scoredPostings(grouped)
    val vectors = testObject.vectorPostings(scored)
    assert(vectors.filter(tuple => tuple._1 == 350000 && tuple._2 == 67).collect.length > 0)
    assert(vectors.filter(tuple => tuple._1 == 100000 && tuple._2 == 89).collect.length > 0)
    assert(vectors.filter(tuple => tuple._1 == 300000 && tuple._2 == 3).collect.length > 0)
    assert(vectors.filter(tuple => tuple._1 == 50000 && tuple._2 == 30).collect.length > 0)
    assert(vectors.filter(tuple => tuple._1 == 200000 && tuple._2 == 20).collect.length > 0)
    println("vectors:")
    vectors.collect.foreach(println(_))
  }

  @Test def `test kmeans`: Unit = {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored = testObject.scoredPostings(grouped)
    val vectors = testObject.vectorPostings(scored)
    val means   = testObject.kmeans(testObject.sampleVectors(vectors), vectors, debug = true)
  }

  @Test def `test clusterResults`: Unit = {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored = testObject.scoredPostings(grouped)
    val vectors = testObject.vectorPostings(scored)
    println("vectors:")
    vectors.collect.foreach(println(_))
    val means   = testObject.kmeans(testObject.sampleVectors(vectors), vectors, debug = true)
    println("means:")
    means.foreach(println(_))
    val results = testObject.clusterResults(means, vectors)
    println("results:")
    results.foreach(println(_))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
