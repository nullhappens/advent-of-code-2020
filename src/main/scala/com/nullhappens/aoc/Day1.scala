package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import fs2.io._
import fs2.text
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.Applicative
import cats.effect.Console.implicits._

object Main extends IOApp {
  implicit def unsafeLogger = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        lines <- loadSolutionFile[IO]("/day1.txt", blocker).compile.toList
        intList <- IO(transform(lines))
        _ <- solutionPart1[IO](intList)
      } yield (ExitCode.Success)
    }
  }

  def transform(lst: List[String]): List[Int] =
    lst.map(s => Either.catchNonFatal(Integer.parseInt(s)).toOption).flatten

  def solutionPart1[F[_]: Applicative: Logger: Console](
      lst: List[Int]
    ): F[Unit] =
    lst.zipWithIndex.traverse_ {
      case (x, i) =>
        lst
          .drop(i + 1)
          .find(y => x + y == 2020)
          .fold(Applicative[F].unit)(y =>
            Console[F].putStrLn(
              s"PART 1: found matches for $x and $y, thus the answer is ${x * y}"
            )
          )
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
