package com.github.sagifogel.freemanfunctors.data

/**
  * Fold takes a way of monoidaly analyzing a B and will give you back a way of monoidaly analyzing an A
  */
final case class Fold[M, A, B](runFold: (B => M) => A => M)