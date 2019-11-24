package com.github.sagifogel.freemanfunctors.instances

import cats.{Comonad, Functor}
import cats.arrow.{Profunctor, Strong}
import com.github.sagifogel.freemanfunctors.data.{Costar, Fold, Forget, Star}

object StrongInstances {
  implicit def functionStrong(implicit P: Profunctor[* => *]): Strong[Function1] = new Strong[* => *] {
    override def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
      case (a, c) => (fa(a), c)
    }

    override def second[A, B, C](fa: A => B): ((C, A)) => (C, B) = {
      case (c, a) => (c, fa(a))
    }

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = P.dimap(fab)(f)(g)
  }

  implicit def forgetStrong[R](implicit P: Profunctor[Forget[R, *, *]]): Strong[Forget[R, *, *]] =
    new Strong[Forget[R, *, *]] {
      override def first[A, B, C](fa: Forget[R, A, B]): Forget[R, (A, C), (B, C)] =
        Forget { case (a, _) => fa.runForget(a) }

      override def second[A, B, C](fa: Forget[R, A, B]): Forget[R, (C, A), (C, B)] = {
        Forget { case (_, a) => fa.runForget(a) }
      }

      override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
        P.dimap(fab)(f)(g)
    }

  implicit def starStrong[F[_]](implicit P: Profunctor[Star[F, *, *]], F: Functor[F]): Strong[Star[F, *, *]] =
    new Strong[Star[F, *, *]] {
      override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] =
        Star { case (a, c) => F.map(fa.runStar(a))((_, c)) }

      override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] =
        Star { case (c, a) => F.map(fa.runStar(a))((c, _)) }

      override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
        P.dimap(fab)(f)(g)
    }

  implicit def costarStrong[F[_]](implicit P: Profunctor[Costar[F, *, *]], W: Comonad[F]): Strong[Costar[F, *, *]] =
    new Strong[Costar[F, *, *]] {
      override def first[A, B, C](costar: Costar[F, A, B]): Costar[F, (A, C), (B, C)] =
        Costar(fac => {
          val fa = W.map(fac)(_._1)
          val fbc = W.map(fac) { case (_, c) => (costar.runStar(fa), c) }

          W.extract(fbc)
        })

      override def second[A, B, C](costar: Costar[F, A, B]): Costar[F, (C, A), (C, B)] =
        Costar(fca => {
          val fa  = W.map(fca)(_._2)
          val fcb = W.map(fca) { case(c, _) => (c, costar.runStar(fa)) }

          W.extract(fcb)
        })

      override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
        P.dimap(fab)(f)(g)
    }

   def foldStrong[M](implicit P: Profunctor[Fold[M, *, *]]): Strong[Fold[M, *, *]] = new Strong[Fold[M, *, *]] {
     override def first[A, B, C](fa: Fold[M, A, B]): Fold[M, (A, C), (B, C)] = {
       Fold(k => {
        case (a, c) => fa.runFold(b => k((b, c)))(a)
       })
     }

     override def second[A, B, C](fa: Fold[M, A, B]): Fold[M, (C, A), (C, B)] =
       Fold(k => {
         case (c, a) => fa.runFold(b => k((c, b)))(a)
       })

     override def dimap[A, B, C, D](fab: Fold[M, A, B])(f: C => A)(g: B => D): Fold[M, C, D] =
       P.dimap(fab)(f)(g)
   }
}
