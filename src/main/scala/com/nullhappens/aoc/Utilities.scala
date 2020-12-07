package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import fs2.text
import fs2.io._

object Utilities {

  def loadSolutionFile[F[_]: Sync: ContextShift](
      fileName: String,
      blocker: Blocker
    ): fs2.Stream[F, String] =
    readInputStream(
      Sync[F].delay(getClass().getResourceAsStream(fileName)),
      4096,
      blocker,
      true
    ).through(text.utf8Decode)
      .through(text.lines)

  def parseInt(s: String): Option[Int] =
    Either.catchNonFatal(Integer.parseInt(s)).toOption
}
