package com.nullhappens.aoc

import java.util.concurrent.TimeUnit

import cats.effect.Console.implicits._
import cats.effect._
import cats.implicits._

object Day9 extends IOApp {

  def testList(lst: List[Long], value: Long): Option[(Long, Long)] = {
    (0 until lst.length).toList.flatMap { i =>
      (1 until lst.length).toList.flatMap { j =>
        val slice = lst.slice(i, j).sorted
        if (slice.sum === value && slice.length > 1) {
          for {
            h <- slice.headOption
            l <- slice.lastOption
          } yield ((h, l))
        } else None
      }
    }.headOption
  }

  def exists(test: Long, lst: List[Long]): Boolean =
    (0 until lst.length).exists { i =>
      (i + 1 until lst.length).exists(j => lst(i) + lst(j) == test)
    }

  def findPosition(preambleSize: Int, list: List[Long]): Option[Int] =
    (preambleSize until list.length).find {
      case i =>
        val x = list(i)
        val lhs = list.slice(i - preambleSize, i)
        !exists(x, lhs)
    }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        lines <- Utilities
          .loadSolutionFile[IO]("/day9.txt", blocker)
          .map(Utilities.parseLong)
          .compile
          .toList
          .map(_.flatten)
        preamble = 25
        position = findPosition(preamble, lines)
        value = position.map(lines(_))
        pair = value.flatMap(v => testList(lines, v))
        _ <- Console[IO].putStrLn(s"Part 1: ${value}")
        _ <- Console[IO].putStrLn(
          s"Part 2: ${pair.map { case (x, y) => x + y }}"
        )
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

}
