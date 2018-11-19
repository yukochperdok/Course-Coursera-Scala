package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    require(a()!=0,"signal a could not be zero")
    Signal(Math.pow(b(),2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    require(a()!=0,"signal a could not be zero")
    Signal {
      delta match {
        case d if (d() < 0) => Set.empty[Double]
        case d if (d() == 0) => Set(-b() / 2 * a())
        case d /*if (d() > 0)*/ =>
          Set(
            -b() + Math.sqrt(d()) / 2 * a(),
            -b() - Math.sqrt(d()) / 2 * a()
          )
      }
    }
  }
}
