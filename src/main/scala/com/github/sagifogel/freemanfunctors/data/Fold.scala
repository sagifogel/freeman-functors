package com.github.sagifogel.freemanfunctors.data

case class Fold[M, A, B](runFold: (B => M) => A => M)