package matryoshka

import scalaz._
import slamdata.Predef

object TestMatryoshka extends App {

  import matryoshka.implicits._
  import matryoshka.data._
  import matryoshka.patterns._

  sealed trait Expr[A]

  case class Add[A](expr1: A, expr2: A) extends Expr[A]

  case class Mult[A](expr1: A, expr2: A) extends Expr[A]

  case class Num[A](literal: Int) extends Expr[A]

  //  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
  //    def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
  //      case Add(e1, e2) => Add(f(e1), f(e2))
  //      case Mult(e1, e2) => Mult(f(e1), f(e2))
  //      case x@Num(_) => x
  //    }
  //  }

  implicit val expTraverse: Traverse[Expr] = new Traverse[Expr] {
    def traverseImpl[G[_], A, B](fa: Expr[A])(f: A => G[B])(implicit G: Applicative[G]): G[Expr[B]] = fa match {
      case Add(e1, e2) => G.apply2(f(e1), f(e2))(Add(_, _))
      case Mult(e1, e2) => G.apply2(f(e1), f(e2))(Mult(_, _))
      case Num(c) => G.pure(Num(c))
    }
  }


  implicit def expShow[T]: Show[Expr[T]] = new Show[Expr[T]] {
    override def shows(e: Expr[T]): String = e.toString
  }

  val exprAlgebra: Algebra[Expr, Int] = {
    case Add(x1, x2) => x1 + x2
    case Mult(x1, x2) => x1 * x2
    case Num(x) => x
  }

  // Evaluate an expression`
  def eval[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Int = e.cata[Int](exprAlgebra)

  def size[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Int = e.cata[Int](matryoshka.size)

  def attr[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Cofree[Expr, Int] = e.cata(matryoshka.attributeAlgebra(exprAlgebra))

  def tree[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Tree[Expr[Predef.Unit]] = e.cata(matryoshka.toTree)

  val gg: Algebra[Expr, Cofree[Expr, Int]] = matryoshka.attributeAlgebra(exprAlgebra)

  def foo[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Int = T.histo[Int](e) {
    case _: Expr[Cofree[Expr, Int]] => 1
  }



  def expr[T](implicit T: Corecursive.Aux[T, Expr]): T =
    Add(
      Mult(
        Num[T](2).embed,
        Num[T](3).embed
      ).embed,
      Num[T](3).embed
    ).embed


  def unwind[T](x: Int)(implicit T: Corecursive.Aux[T, Expr]): T = x.ana[T].apply[Expr] {
    case x if x <= 2 => Num(x)
    case x if x % 2 == 0 => Mult(x / 2, 2)
    case x => Add(x - 1, 1)
  }


  val unwindExpr: Fix[Expr] = unwind[Fix[Expr]](11)
  val annotatedExpr: Cofree[Expr, Int] = attr(unwindExpr)

  println(unwindExpr)
  println(eval(unwindExpr))
  println(tree(unwindExpr).drawTree)
  println(annotatedExpr)
  //  println(s"size = ${size(unwindExpr)}")

//  println(eval(annotatedExpr))
  implicitly[Recursive.Aux[Cofree[Expr, Int], EnvT[Int, Expr, ?]]]
  println(annotatedExpr.head)
}

