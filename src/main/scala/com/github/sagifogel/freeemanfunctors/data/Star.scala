package com.github.sagifogel.freeemanfunctors.data

import com.github.sagifogel.freeemanfunctors.syntax.Function1Syntax._
import cats.Functor

final case class Star[F[_], A, B](runStar: A => F[B]) {
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Star[F, C, D] = {
    val flipMap = F.fmap[B, D] _ flip

    Star(flipMap(g) compose runStar compose f)
  }
}