package com.github.sagifogel.freeemanfunctors.data

final case class Forget[R, A, B](runForget: A => R)