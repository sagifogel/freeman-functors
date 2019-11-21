package com.github.sagifogel.freemanfunctors.data

import cats.arrow.Arrow

final case class Mealy[A, B](runMealy: A => (B, Mealy[A, B])) { self =>
  def dimap[C, D](f: C => A)(g: B => D)(implicit A: Arrow[Mealy]): Mealy[C, D] = {
    A.dimap(self)(f)(g)
  }
}
