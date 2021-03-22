package scalashop

import java.util.concurrent._
import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var yc = 0
    while (yc < src.height) {
      var xc = from
      while (xc < end) {
        dst.update(xc, yc, boxBlurKernel(src, xc, yc, radius))
        xc += 1
      }
      yc += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def run_tasks(start_end_pairs: IndexedSeq[(Int, Int)], tasks: List[ForkJoinTask[Unit]]): List[ForkJoinTask[Unit]] = {
      start_end_pairs match {
        case IndexedSeq() => tasks
        case (from, _) +: IndexedSeq() => run_tasks(start_end_pairs.tail, task(blur(src, dst, from, src.width, radius)) :: tasks)
        case (from, end) +: _ => run_tasks(start_end_pairs.tail, task(blur(src, dst, from, end, radius)) :: tasks)
      }
    }
    val froms = 0 to src.width by ((src.width / numTasks) max 1)
    val start_end_pairs = froms zip froms.tail
    val tasks = run_tasks(start_end_pairs, Nil)
    for (task <- tasks) {
      task.join()
    }
  }
}
