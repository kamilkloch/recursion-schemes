package manual

import scalaz.Scalaz._
import scalaz._

object HigherOrder extends App {

  sealed abstract class Expr[+A[_], I] extends Product with Serializable

  case class Pair[A[_], I, J](_1: A[I], _2: A[J]) extends Expr[A, (I, J)]

  case class Const[A[_]](value: Int) extends Expr[Nothing, Int]

  case class Addd[A[_]](a: A[Int], b: A[Int]) extends Expr[A, Int]

  case class _1[A[_], I, J](x: A[(I, J)]) extends Expr[A, I]

  case class _2[A[_], I, J](x: A[(I, J)]) extends Expr[A, J]


  trait HFunctor[F[_[_], _]] {
    def hfmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
  }

  object HFunctor {
    def apply[F[_[_], _]](implicit v: HFunctor[F]): HFunctor[F] = v

    final implicit class HFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: HFunctor[F]) {
      def hfmap[N[_]](nt: M ~> N): F[N, A] = F.hfmap(nt)(fa)
    }

    type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
  }

  final case class HFix[F[_[_], _], I](unfix: F[HFix[F, ?], I])

  object HFix {

    import HFunctor._

    def hfix[F[_[_], _], I](fa: => F[HFix[F, ?], I]): HFix[F, I] =
      HFix[F, I](fa)

    def cataNT[F[_[_], _] : HFunctor, G[_]](alg: HAlgebra[F, G]): (HFix[F, ?] ~> G) =
      new (HFix[F, ?] ~> G) {
        self =>
        def apply[I](f: HFix[F, I]): G[I] = {
          alg.apply[I](f.unfix.hfmap[G](self))
        }
      }
  }

  implicit def expr2HFunctor: HFunctor[Expr] = new HFunctor[Expr] {
    def hfmap[M[_], N[_]](nt: M ~> N): Expr[M, ?] ~> Expr[N, ?] =
      new (Expr[M, ?] ~> Expr[N, ?]) {
        def apply[I](f: Expr[M, I]): Expr[N, I] = f match {
          case x@Const(_) => x
          case _ => ???
        }
      }
  }

  val expr2: HFix[Expr, Int] = HFix[Expr, Int](Const(3))

  def eval2(e: HFix[Expr, Int]) = HFix.cataNT[Expr, Id](new (Expr[Id, ?] ~> Id) {
    def apply[A](e: Expr[Id, A]): Id[A] = e match {
      case Const(c) => c
      case _ => ???
    }
  }
  ).apply(e)

  val hh = eval2(expr2)
  println(hh)


}
