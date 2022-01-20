object MonoidsInCategoryOfEndofunctors {
  trait FunctionalMonoid[T] {
    def unit: Unit => T
    def combine: ((T, T)) => T
  }

  // monoid
  trait Monoid[T] extends FunctionalMonoid[T] {
    // "public" API
    def empty: T
    def combine(a: T, b: T): T

    // "hidden" API
    def unit = _ => empty
    def combine = t => combine(t._1, t._2)
  }

  object IntMonoidAdd extends Monoid[Int] {
    override def empty: Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  def main(args: Array[String]): Unit = {
    println("main")
  }
}
