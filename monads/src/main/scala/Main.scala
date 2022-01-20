object MonoidsInCategoryOfEndofunctors {
  // monoid
  trait Monoid[T] {
    def empty: T
    def combine(a: T, b: T): T
  }

  object IntMonoidAdd extends Monoid[Int] {
    override def empty: Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  def main(args: Array[String]): Unit = {
    println("main")
  }
}
