package scala

import org.junit.Assert._

object DoubleSuite extends tests.Suite {
  test("toInt") {
    @inline
    def test(x: Double, expected: Int): Unit =
      assertEquals(expected, x.toInt)

    // Specials
    test(+0.0, 0)
    test(-0.0, 0)
    test(Double.PositiveInfinity, Int.MaxValue)
    test(Double.NegativeInfinity, Int.MinValue)
    test(Double.NaN, 0)

    // Positive numbers
    test(0.3, 0)
    test(0.7, 0)
    test(1.2, 1)
    test(5e12, Int.MaxValue)
    test(2147483646, 2147483646)
    test(2147483646.999, 2147483646)
    test(2147483512.546, 2147483512)
    test(65.67, 65)

    // Negative numbers
    test(-0.3, 0)
    test(-0.7, 0)
    test(-1.2, -1)
    test(-5e12, Int.MinValue)
    test(-2147483647.9999, -2147483647)
    test(-2147483565.123, -2147483565)
    test(-65.67, -65)
  }
}
