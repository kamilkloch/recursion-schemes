package matryoshka

import matryoshka._
import matryoshka.implicits._
import matryoshka.data._
import matryoshka.patterns._
import scalaz._
import Scalaz._


sealed trait DoubleValue
final case class PriceyDouble(v: Double) extends DoubleValue
final case class NormalDouble(v: Double) extends DoubleValue

sealed trait Expr[A]

case class Add[A](expr1: A, expr2: A) extends Expr[A]

case class Mult[A](expr1: A, expr2: A) extends Expr[A]

case class Num[A](v: DoubleValue) extends Expr[A]


object Expr {

  implicit val expTraverse: Traverse[Expr] = new Traverse[Expr] {
    def traverseImpl[G[_], A, B](fa: Expr[A])(f: A => G[B])(implicit G: Applicative[G]): G[Expr[B]] = fa match {
      case Add(e1, e2) => G.apply2(f(e1), f(e2))(Add(_, _))
      case Mult(e1, e2) => G.apply2(f(e1), f(e2))(Mult(_, _))
      case Num(c) => G.pure(Num(c))
    }
  }

}


object Generator extends App {

  def cofreeIso[E, W[_]]: AlgebraIso[EnvT[E, W, ?], Cofree[W, E]] = {
    val alg: Algebra[EnvT[E, W, ?], Cofree[W, E]] = { e: EnvT[E, W, Cofree[W, E]] =>
      val x: E = e.ask
      val y: W[Cofree[W, E]] = e.lower
      Cofree(x, y)
    }

    val coalg: Coalgebra[EnvT[E, W, ?], Cofree[W, E]] = { e: Cofree[W, E] =>
      val x: E = e.head
      val y: W[Cofree[W, E]] = e.tail
      EnvT((x, y))
    }

    AlgebraIso(alg)(coalg)
  }

  val iso = cofreeIso[Int, Expr]



  //  val expr = Add(Fix(Mult(Num(3.0)), Num(2.0).embed), Num(1.0).embed).embed
  val expr: Fix[Expr] = Fix(Add(
    Fix(Num(PriceyDouble(1.0))),
    Fix(Num(PriceyDouble(2.0)))
  ))


  val res: DoubleValue = expr.cata[DoubleValue] {
    case Add(PriceyDouble(x), PriceyDouble(y)) => PriceyDouble(x + y)
    case Add(PriceyDouble(x), PriceyDouble(y)) => PriceyDouble(x + y)
  }

  println(res)

}
