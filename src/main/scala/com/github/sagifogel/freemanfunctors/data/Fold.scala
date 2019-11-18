package com.github.sagifogel.freemanfunctors.data

final case class Fold[M, A, B](runFold: (B => M) => A => M)