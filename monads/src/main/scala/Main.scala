object MonoidsInCategoryOfEndofunctors {
  trait MostAbstractMonoid[T, ~>[_, _], U, P] {
    def unit: U ~> T
    def combine: P ~> T
  }

  trait GeneralMonoid[T, U, P] extends MostAbstractMonoid[T, Function1, U, P]

  trait FunctionalMonoid[T] extends GeneralMonoid[T, Unit, (T, T)]

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
