package com.github.sagifogel.freemanfunctors.instances

import cats.arrow.Arrow
import com.github.sagifogel.freemanfunctors.data.Mealy

object ArrowInstances {
  implicit val mealyArrow: Arrow[Mealy] = new Arrow[Mealy] {
    override def lift[A, B](f: A => B): Mealy[A, B] = Mealy(a => (f(a), lift(f)))

    override def compose[A, B, C](f: Mealy[B, C], g: Mealy[A, B]): Mealy[A, C] =
      Mealy[A, C](a => {
        val (x, mealyg) = g.runMealy(a)
        val (c, mealyf) = f.runMealy(x)

        (c, compose(mealyf, mealyg))
      })

    override def first[A, B, C](fa: Mealy[A, B]): Mealy[(A, C), (B, C)] =
      Mealy[(A, C), (B, C)] { case (a, c) =>
        val (b, mealy) = fa.runMealy(a)

        ((b, c), first(mealy))
      }
  }
}
