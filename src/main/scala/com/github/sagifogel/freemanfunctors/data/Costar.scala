package com.github.sagifogel.freemanfunctors.data

import com.github.sagifogel.freemanfunctors.syntax.Function1Syntax._
import cats.Functor

final case class Costar[F[_], A, B](runStar: F[A] => B) {
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Costar[F, C, D] = {
    val flipMap = F.fmap[C, A] _ flip

    Costar(g compose runStar compose flipMap(f))
  }
}
