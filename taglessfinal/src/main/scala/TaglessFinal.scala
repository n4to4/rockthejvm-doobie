object TaglessFinal {
  // expression problem
  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean) extends Expr
    case class Or(left: Expr, right: Expr) extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(expr: Expr) extends Expr

    val aGiantBoolean: Expr = Or(And(B(true), B(false)), B(false))

    def eval(expr: Expr): Boolean = expr match {
      case B(b)      => b
      case Or(a, b)  => eval(a) || eval(b)
      case And(a, b) => eval(a) && eval(b)
      case Not(e)    => !eval(e)
    }

    // includes ints
    case class I(int: Int) extends Expr
    case class Sum(left: Expr, right: Expr) extends Expr

    def eval_v2(expr: Expr): Any = expr match {
      case B(b) => b
      case Or(a, b) =>
        eval(a).asInstanceOf[Boolean] || eval(b).asInstanceOf[Boolean]
      // casts everywhere
    }
  }

  object Tagging {
    trait Expr(val tag: String)
    case class B(boolean: Boolean) extends Expr("bool")
    case class Or(left: Expr, right: Expr) extends Expr("bool")
    case class And(left: Expr, right: Expr) extends Expr("bool")
    case class Not(expr: Expr) extends Expr("bool")
    case class I(int: Int) extends Expr("int")
    case class Sum(left: Expr, right: Expr) extends Expr("int")

    def eval(expr: Expr): Any = expr match {
      case B(b) => b
      case Or(left, right) =>
        if (left.tag != "bool" || right.tag != "bool")
          throw new IllegalArgumentException("improper argument type")
        else
          eval(left).asInstanceOf[Boolean] ||
          eval(right).asInstanceOf[Boolean]
      // same
    }
  }

  object TaglessInitial {
    trait Expr[A]
    case class B(boolean: Boolean) extends Expr[Boolean]
    case class Or(left: Expr[Boolean], right: Expr[Boolean])
        extends Expr[Boolean]
    case class And(left: Expr[Boolean], right: Expr[Boolean])
        extends Expr[Boolean]
    case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
    case class I(int: Int) extends Expr[Int]
    case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def eval[A](expr: Expr[A]): A = expr match {
      case B(b)             => b
      case I(i)             => i
      case Or(left, right)  => eval(left) || eval(right)
      case Sum(left, right) => eval(left) + eval(right)
      // etc
    }
  }

  object TaglessFinal {
    trait Expr[A] {
      val value: A
    }

    def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
      val value = boolean
    }

    def i(int: Int): Expr[Int] = new Expr[Int] {
      val value = int
    }

    def and(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] =
      new Expr[Boolean] {
        val value = left.value && right.value
      }

    def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] =
      new Expr[Boolean] {
        val value = left.value || right.value
      }

    def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] = new Expr[Int] {
      val value = left.value + right.value
    }

    def eval[A](expr: Expr[A]): A = expr.value
  }

  def demoTagless(): Unit = {
    import TaglessInitial._
    println(eval(Or(B(true), And(B(true), B(false)))))
    println(eval(Sum(I(24), I(-3))))
  }

  def demoTaglessFinal(): Unit = {
    import TaglessFinal._
    println(eval(or(b(true), and(b(true), b(false)))))
    println(eval(sum(i(24), i(-3))))
  }

  def main(args: Array[String]): Unit = {
    demoTaglessFinal()
  }
}
