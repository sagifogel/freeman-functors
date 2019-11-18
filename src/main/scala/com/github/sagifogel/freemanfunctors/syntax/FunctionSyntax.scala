package com.github.sagifogel.freemanfunctors.syntax

object Function1Syntax {
  implicit class Function1Ops[A, B, C](val f: A => B => C) extends AnyVal {
    def flip: B => A => C = b => a => f(a)(b)
  }
}
