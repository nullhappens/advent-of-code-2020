package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import cats.effect.Console.implicits._
import java.util.concurrent.TimeUnit

object Day6 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        lines <- Utilities
          .loadSolutionFile[IO]("/day6.txt", blocker)
          // shamelessly stolen from
          // https://medium.com/se-notes-by-alexey-novakov/scala-fs2-handle-broken-csv-lines-8d8c4defcd88
          .scan(("", "")) {
            case ((acc, _), line) =>
              if (line.isEmpty()) (line, acc) else (s"$acc $line", "")
          }
          .collect {
            case (_, line) if line.trim.nonEmpty => line.trim()
          }
          .compile
          .toList
        _ <- Console[IO].putStrLn(
          s"Part1: ${lines.map(_.replaceAll("\\s", "").toSet.size).sum}"
        )
        _ <- Console[IO].putStrLn(
          s"Part2: ${lines.map { s =>
            s.split(" ").map(_.toSet).reduce((a, b) => a.intersect(b)).size
          }.sum}"
        )
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

}
