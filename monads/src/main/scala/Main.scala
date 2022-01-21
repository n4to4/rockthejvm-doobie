object MonoidsInCategoryOfEndofunctors {
  // "monoid in the category of T[_]"
  trait MonoidInCategoryK2[T[_], ~>[_[_], _[_]], U[_], P[_]] {
    def unit: U ~> T // same as ~>[U, T]
    def combine: P ~> T
  }

  // "monoid in a monoidal category" = "monoid in the category of T"
  trait MonoidInCategory[T, ~>[_, _], U, P] {
    def unit: U ~> T // same as ~>[U, T]
    def combine: P ~> T
  }

  trait GeneralMonoid[T, U, P] extends MonoidInCategory[T, Function1, U, P] {
    // def unit: U => T
    // def combine: P => T
  }

  trait FunctionalMonoid[T] extends GeneralMonoid[T, Unit, (T, T)] {
    // def unit: Unit => T
    // def combine: ((T, T)) => T
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

  // endofunctors
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  given functorList: Functor[List] with {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // functor transformations
  trait MyFunction1[-A, +B] {
    def apply(a: A): B
  }

  trait FunctorNatTransformation[-F[_], +G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object ListToOptionTrans extends FunctorNatTransformation[List, Option] {
    override def apply[A](fa: List[A]): Option[A] = fa.headOption
  }

  // .toList, .toOption, .toEither, .toTry

  // the id functor
  type Id[A] = A

  given idFunctor: Functor[Id] with {
    def map[A, B](fa: A)(f: A => B): B = f(fa)
  }

  // composing functors
  def funcComposition[A, B, C](f: A => B, g: B => C, x: A) = g(f(x))
  type HKTComposition[F[_], G[_], A] = G[F[A]]
  type SameTypeComposition[F[_], A] = F[F[A]]

  trait MonoidInCategoryOfFunctors[F[_]: Functor]
      extends MonoidInCategoryK2[
        F,
        FunctorNatTransformation,
        Id,
        [A] =>> F[F[A]]
      ] {
    type FunctorProduct[A] = F[F[A]]
    // compiler knows:
    // def unit: FunctorNatTransformation[Id, F]
    // def combine: FunctorNatTransformation[FunctorProduct, F]

    // MonoidInCategoryOfFunctors == Monads
    def pure[A](a: A): F[A] = unit(a)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = {
      val functor = summon[Functor[F]] // implicitly
      val ffb: F[F[B]] = functor.map(fa)(f)
      combine(ffb)
    }
  }

  object ListSpecialMonoid extends MonoidInCategoryOfFunctors[List] {
    def unit: FunctorNatTransformation[Id, List] =
      new FunctorNatTransformation[Id, List] {
        def apply[A](fa: Id[A]): List[A] = List(fa)
      }

    def combine: FunctorNatTransformation[FunctorProduct, List] =
      new FunctorNatTransformation[FunctorProduct, List] {
        def apply[A](fa: List[List[A]]): List[A] = fa.flatten
      }
  }

  def main(args: Array[String]): Unit = {
    println(
      ListSpecialMonoid.combine(
        List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9)
        )
      )
    )
  }
}
