import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

sealed trait Expr[+A]

case class Add[A](expr1: A, expr2: A) extends Expr[A]

case class Mult[A](expr1: A, expr2: A) extends Expr[A]

case class Num(literal: Int) extends Expr[Nothing]

object RecursionSchemes extends App {

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
      case Add(e1, e2) => Add(f(e1), f(e2))
      case Mult(e1, e2) => Mult(f(e1), f(e2))
      case x@Num(_) => x
    }
  }

  // Fixed point type
  case class Fix[F[_]](unfix: F[Fix[F]])

  // Catamorphism
  def cata[F[_] : Functor, A](structure: Fix[F])(algebra: F[A] => A): A = {
    // algebra(structure.unfix.map(cata(_)(algebra)))
    val fr: F[Fix[F]] = structure.unfix
    val fa: F[A] = fr.map((a: Fix[F]) => cata(a)(algebra))
    val a = algebra(fa)
    a
  }

  // Anamorphism
  def ana[F[_] : Functor, A](value: A)(coalgebra: A => F[A]): Fix[F] = {
    // Fix(coalgebra(value).map(ana(_)(coalgebra)))
    val fa: F[A] = coalgebra(value)
    val fr: F[Fix[F]] = fa.map((a: A) => ana(a)(coalgebra))
    val r: Fix[F] = Fix(fr)
    r
  }

  // Evaluate an expression
  def eval(e: Fix[Expr]): Int = cata[Expr, Int](e) {
    case Add(x1, x2) => x1 + x2
    case Mult(x1, x2) => x1 * x2
    case Num(x) => x
  }

  def unwind(x: Int): Fix[Expr] = ana[Expr, Int](x) {
    case x if x <= 2 => Num(x)
    case x if x % 2 == 0 => Mult(x / 2, 2)
    case x => Add(x - 1, 1)
  }

  val expr: Fix[Expr] =
    Fix(Add(
      Fix(Mult(
        Fix[Expr](Num(2)),
        Fix[Expr](Num(3))
      )),
      Fix[Expr](Num(3))
    ))

  val exprRes = eval(expr)
  println(exprRes) // 9

  val unwindRes = unwind(124534)
  println(unwindRes)
  println(eval(unwindRes))
}

object RecursionSchemesMatryoshka extends App {

  import matryoshka.data.Fix
  import matryoshka.implicits._
  import matryoshka.{Corecursive, Recursive} // Syntax

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
      case x@Num(_) => G.pure(x)
    }
  }

  implicit def expShow[T]: Show[Expr[T]] = new Show[Expr[T]] {

  }

  // Evaluate an expression
  def eval[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Int = e.cata[Int] {
    case Add(x1, x2) => x1 + x2
    case Mult(x1, x2) => x1 * x2
    case Num(x) => x
  }

  def size[T](e: T)(implicit T: Recursive.Aux[T, Expr]): Int = e.cata[Int](matryoshka.size)

  def tree[T](e: T)(implicit T: Recursive.Aux[T, Expr]) = e.cata(matryoshka.toTree)


  def expr[T](implicit T: Corecursive.Aux[T, Expr]): T =
    Add(
      Mult(
        Num(2).embed,
        Num(3).embed
      ).embed,
      Num(3).embed
    ).embed


  def unwind[T](x: Int)(implicit T: Corecursive.Aux[T, Expr]): T = x.ana[T].apply[Expr] {
    case x if x <= 2 => Num(x)
    case x if x % 2 == 0 => Mult(x / 2, 2)
    case x => Add(x - 1, 1)
  }


  val unwindExpr = unwind[Fix[Expr]](123235)

  println(unwindExpr)
  println(eval(unwindExpr))
//  println(tree(unwindExpr).drawTree)
//  println(s"size = ${size(unwindExpr)}")
}