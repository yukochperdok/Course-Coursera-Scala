package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for(row <- from until end){
      for(col <- 0 until src.width)
        dst.update(col, row, scalashop.boxBlurKernel(src, col, row, radius))
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val numRowPerTask: Int =
      if(src.height % numTasks == 0) src.height / numTasks
      else (src.height / numTasks) + 1
    val listFromEnd: List[Int] = (0 to src.height by numRowPerTask).toList
    val tuplesFromEnd: List[(Int, Int)] = listFromEnd zip listFromEnd.tail

    val tasks: Seq[ForkJoinTask[Unit]] =
      for (listTuples <- tuplesFromEnd)
        yield task{
          blur(src, dst, listTuples._1, listTuples._2, radius)
        }

    println("About to wait for some heavy calculation...")
    tasks.foreach(_.join())
  }

}
