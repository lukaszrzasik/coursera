package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.junit.{Assert, Test}
import org.junit.Assert.assertEquals

import scala.util.Random

class TimeUsageSuite {
  import TimeUsage._

  def convertRowToListOfStrings(row: Seq[Any]): List[String] =
    if (row.isEmpty) Nil
    else row.head.toString :: convertRowToListOfStrings(row.tail)

  @Test def `row test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val rowstring = convertRowToListOfStrings(initDf.first.toSeq)
    println(row(rowstring).schema)
    assert(true)
  }

  @Test def `classifiedColumns test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    println(classCols.toString)
    assert(true)
  }

  @Test def `timeUsageSummary test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    val summary = timeUsageSummary(classCols._1, classCols._2, classCols._3, initDf)
    summary.show
    assert(true)
  }

  @Test def `timeUsageGrouped test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    val summary = timeUsageSummary(classCols._1, classCols._2, classCols._3, initDf)
    val grouped = timeUsageGrouped(summary)
    grouped.show
    assert(true)
  }

  @Test def `timeUsageGroupedSqlQuery test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    val summary = timeUsageSummary(classCols._1, classCols._2, classCols._3, initDf)
    val grouped = timeUsageGroupedSql(summary)
    grouped.show
    assert(true)
  }

  @Test def `timeUsageSummaryTyped test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    val summary = timeUsageSummary(classCols._1, classCols._2, classCols._3, initDf)
    val ds = timeUsageSummaryTyped(summary)
    ds.show
    ds.printSchema
    assert(true)
  }

  @Test def `timeUsageGroupedTyped test`: Unit = {
    val (columns, initDf) = read("src/test/resources/timeusage/atussum-test.csv")
    val classCols = classifiedColumns(columns)
    val summary = timeUsageSummary(classCols._1, classCols._2, classCols._3, initDf)
    val ds = timeUsageSummaryTyped(summary)
    val dsGrouped = timeUsageGroupedTyped(ds)
    dsGrouped.show
    dsGrouped.printSchema
    assert(true)
  }
}
