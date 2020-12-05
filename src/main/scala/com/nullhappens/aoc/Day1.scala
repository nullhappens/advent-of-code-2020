package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import fs2.io._
import fs2.text
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.Applicative

object Main extends IOApp {
  implicit def unsafeLogger = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        lines <- loadSolutionFile[IO]("/day1.txt", blocker).compile.toList
        _ <- findSolution[IO](lines)
      } yield (ExitCode.Success)
    }
  }

  def findSolution[F[_]: Applicative: Logger](lst: List[String]): F[Unit] = {
    val intList =
      lst.map(s => Either.catchNonFatal(Integer.parseInt(s)).toOption).flatten

    intList.zipWithIndex.traverse_ {
      case (x, i) =>
        intList
          .drop(i + 1)
          .find(y => x + y == 2020)
          .fold(Applicative[F].unit)(y =>
            Logger[F]
              .debug(
                s"found matches for $x and $y, thus the answer is ${x * y}"
              )
          )
    }
  }

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
}
