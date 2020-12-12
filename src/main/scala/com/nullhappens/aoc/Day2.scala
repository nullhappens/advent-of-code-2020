package com.nullhappens.aoc

import scala.collection.immutable.Nil
import scala.util.Try

import cats.effect.Console.implicits._
import cats.effect._
import cats.implicits._

object Day2 extends IOApp {

  final case class PasswordTest(
      min: Int,
      max: Int,
      character: Char,
      password: String) {

    def validPart1(): Boolean = {
      val count = password.count(_ === character)
      count >= min && count <= max
    }

    def validPart2(): Boolean =
      (for {
        a <- Try(password.charAt(min - 1)).toOption
        b <- Try(password.charAt(max - 1)).toOption
      } yield (a != b && (a == character || b == character))).getOrElse(false)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        lines <- Utilities
          .loadSolutionFile[IO]("/day2.txt", blocker)
          .compile
          .toList
        csv <- IO(transform(lines))
        count1 <- IO(csv.count(_.validPart1()))
        count2 <- IO(csv.count(_.validPart2()))
        _ <- Console[IO].putStrLn(s"PART 1: valid password count: $count1")
        _ <- Console[IO].putStrLn(s"PART 2: valid password count: $count2")
      } yield (ExitCode.Success)
    }
  }

  def transform(lst: List[String]): List[PasswordTest] = {
    def getRanges(ranges: String): Option[(Int, Int)] =
      ranges.split("-").toList match {
        case low :: high :: Nil =>
          for {
            a <- Utilities.parseInt(low)
            b <- Utilities.parseInt(high)
          } yield (a, b)
        case _ => None
      }

    def getStringParts(s: String): Option[(String, String, String)] =
      s.split(" ").toList match {
        case ranges :: character :: password :: Nil =>
          Some((ranges, character, password))
        case _ => None
      }

    def getCharacter(s: String): Option[Char] = Try(s.charAt(0)).toOption

    lst.flatMap { s =>
      for {
        (ranges, character, password) <- getStringParts(s)
        (low, high) <- getRanges(ranges)
        char <- getCharacter(character)
      } yield PasswordTest(low, high, char, password)
    }
  }
}
