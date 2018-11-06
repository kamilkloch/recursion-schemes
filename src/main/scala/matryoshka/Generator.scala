package matryoshka

import matryoshka.patterns._
import scalaz._


sealed trait Expr[A]

case class Add[A](expr1: A, expr2: A) extends Expr[A]

case class Mult[A](expr1: A, expr2: A) extends Expr[A]

case class Num[A](literal: Int) extends Expr[A]

object Expr {

  implicit val expTraverse: Traverse[Expr] = new Traverse[Expr] {
    def traverseImpl[G[_], A, B](fa: Expr[A])(f: A => G[B])(implicit G: Applicative[G]): G[Expr[B]] = fa match {
      case Add(e1, e2) => G.apply2(f(e1), f(e2))(Add(_, _))
      case Mult(e1, e2) => G.apply2(f(e1), f(e2))(Mult(_, _))
      case Num(c) => G.pure(Num(c))
    }
  }


}


class Generator {

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


}
