package com.nullhappens.aoc

import cats.Applicative
import cats.effect.Console.implicits._
import cats.effect._
import cats.implicits._

object Day1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        lines <- Utilities
          .loadSolutionFile[IO]("/day1.txt", blocker)
          .compile
          .toList
        intList <- IO(transform(lines))
        _ <- solutionPart1[IO](intList)
        _ <- solutionPart2[IO](intList)
      } yield (ExitCode.Success)
    }
  }

  def transform(lst: List[String]): List[Int] =
    lst.map(s => Either.catchNonFatal(Integer.parseInt(s)).toOption).flatten

  def solutionPart1[F[_]: Applicative: Console](lst: List[Int]): F[Unit] =
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

  def solutionPart2[F[_]: Applicative: Console](lst: List[Int]): F[Unit] =
    lst.zipWithIndex.traverse_ {
      case (x, i) =>
        lst.drop(i + 1).zipWithIndex.traverse_ {
          case (y, j) =>
            lst
              .drop(j + 1)
              .find(z => x + y + z == 2020)
              .fold(Applicative[F].unit)(z =>
                Console[F].putStrLn(
                  s"PART 2: found matches for $x, $y and $z, thus the answer is ${x * y * z}"
                )
              )
        }
    }

}
