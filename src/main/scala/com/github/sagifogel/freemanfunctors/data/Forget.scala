package com.github.sagifogel.freemanfunctors.data

final case class Forget[R, A, B](runForget: A => R)