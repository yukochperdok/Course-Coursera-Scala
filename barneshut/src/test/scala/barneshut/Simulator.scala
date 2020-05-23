package barneshut

import org.junit._

class SimulatorTest {
  // test cases for simulator
  val timeStats = new TimeStatistics
  val taskSupport = new collection.parallel.ForkJoinTaskSupport(
    new java.util.concurrent.ForkJoinPool(4))

  val simulator = new Simulator(taskSupport, timeStats)

  @Test def `Simulator.computeBoundaries: contains all bodies`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)
    val bodies = List(b1, b2, b3)
    val boundaries = simulator.computeBoundaries(bodies)
    assert(boundaries.minX == 18f)
    assert(boundaries.minY == 25.5f)
    assert(boundaries.maxX == 24.5f)
    assert(boundaries.maxY == 41f)
    println(s"boundaries: $boundaries")
  }

  @Test def `Simulator.computeSectorMatrix: contains all bodies and fitting them`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 33.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)
    val b4 = new Body(215f, 20.4f, 31f, 0f, 0f)
    val b5 = new Body(1265f, 30.4f, 36f, 0f, 0f)
    val bodies = List(b1, b2, b3, b4, b5)
    val boundaries = simulator.computeBoundaries(bodies)
    println(s"boundaries: $boundaries")
    println(s"boundaries.width: ${boundaries.width}")
    println(s"boundaries.height: ${boundaries.height}")
    println(s"boundaries.size: ${boundaries.size}")
    val sectorMatrix = simulator.computeSectorMatrix(bodies, boundaries)
    assert(sectorMatrix.boundaries.minX == 18f)
    assert(sectorMatrix.boundaries.minY == 25.5f)
    assert(sectorMatrix.boundaries.maxX == 33.5f)
    assert(sectorMatrix.boundaries.maxY == 41f)
    println(s"sectorMatrix: $sectorMatrix")
    assert(sectorMatrix.matrix.map(_.size).sum == 5)
  }
}

