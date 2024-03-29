package com.github.sagifogel.freemanfunctors.instances

import cats.Functor
import cats.arrow.{Arrow, Profunctor}
import com.github.sagifogel.freemanfunctors.data.{Costar, Fold, Forget, Mealy, Star}

object ProfunctorsInstances {
  implicit def functionProfunctor: Profunctor[Function1] = new Profunctor[* => *] {
    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
      g compose fab compose f
  }

  implicit def forgetProfunctor[R]: Profunctor[Forget[R, *, *]] = new Profunctor[Forget[R, *, *]] {
      override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
        Forget(fab.runForget compose f)
    }

  implicit def starProfunctor[F[_]](implicit F: Functor[F]): Profunctor[Star[F, *, *]] =
    new Profunctor[Star[F, *, *]] {
    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      fab.dimap(f)(g)
  }

  implicit def costarProfunctor[F[_]](implicit F: Functor[F]): Profunctor[Costar[F, *, *]] =
    new Profunctor[Costar[F, *, *]] {
      override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
        fab.dimap(f)(g)
    }

  implicit def foldProfunctor[M]: Profunctor[Fold[M, *, *]] = new Profunctor[Fold[M, *, *]] {
    override def dimap[A, B, C, D](fab: Fold[M, A, B])(f: C => A)(g: B => D): Fold[M, C, D] =
      Fold(k => fab.runFold(k compose g) compose f)
  }

  implicit def mealyProfunctor(implicit A: Arrow[Mealy]): Profunctor[Mealy] = new Profunctor[Mealy] {
    override def dimap[A, B, C, D](fab: Mealy[A, B])(f: C => A)(g: B => D): Mealy[C, D] = {
      fab.dimap(f)(g)
    }
  }
}
