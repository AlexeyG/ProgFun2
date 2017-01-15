package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal
    {
      val deltaResolved = delta()
      if (deltaResolved == 0)
        Set(-b() / (2.0 * a()))
      else if (deltaResolved > 0)
      {
        val x1 = (-b() + math.sqrt(delta())) / (2.0 * a())
        val x2 = (-b() - math.sqrt(delta())) / (2.0 * a())
        Set(x1, x2)
      }
      else
        Set()
    }
  }
}
