package reductions

import org.scalameter._

import scala.annotation.tailrec

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val threshold = 10000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, threshold)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    @tailrec
    def lineOfSightRec(from: Int, input: Array[Float], output: Array[Float]): Unit = {
      if(from < input.length) {
        output(from) = output(from - 1).max(input(from) / from)
        lineOfSightRec(from + 1, input, output)
      }
    }
    output(0) = 0
    if (input.length > 0) lineOfSightRec(1, input, output)
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    val angles = for {
      i <- from until `until`
    } yield {
      if (i == 0) 0
      else input(i) / i
    }
    angles.max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if (end - from < threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val half = from + (end - from) / 2
      val (tL, tR) = parallel(upsweep(input, from, half, threshold), upsweep(input, half, end, threshold))
      Node(tL, tR)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if (from < until && from < input.length) {
      val maxAngle = startingAngle.max(input(from) / from)
      output(from) = maxAngle
      downsweepSequential(input, output, maxAngle, from + 1, until)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match {
      case Leaf(from, end, _) => downsweepSequential(input, output, startingAngle, from, end)
      case Node(l, r) =>
        parallel(
          downsweep(input, output, startingAngle, l),
          downsweep(input, output, startingAngle.max(l.maxPrevious),r))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0f, tree)
  }
}
