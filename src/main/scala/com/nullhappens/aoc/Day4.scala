package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import cats.effect.Console.implicits._
import java.util.concurrent.TimeUnit
import scala.collection.immutable.Nil

object Day4 extends IOApp {

  final case class Passport(
      birthYear: Int,
      issueYear: Int,
      expYear: Int,
      height: String,
      hairColor: String,
      eyeColor: String,
      id: String,
      countryId: Option[String])

  object Passport {

    def parseFromCsv(csv: String): Option[Passport] = {

      def validateNumeric(s: String, low: Int, high: Int): Option[Int] =
        Utilities.parseInt(s).filter(i => i >= low && i <= high)

      def validateHgt(s: String): Option[String] =
        Option(s).filter {
          case s if s.endsWith("cm") =>
            val height = Utilities.parseInt(s.substring(0, s.indexOf("cm")))
            height.forall(i => i >= 150 && i <= 193)
          case s if s.endsWith("in") =>
            val height = Utilities.parseInt(s.substring(0, s.indexOf("in")))
            height.forall(i => i >= 59 && i <= 76)
          case _ => false
        }

      def validateHcl(s: String): Option[String] = {
        val validColor = "^#(?:[0-9a-fA-F]{3}){1,2}$"
        Option(s).filter(_.matches(validColor))
      }

      def validateEcl(s: String): Option[String] = {
        val validL = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
        Option(s).filter(validL.contains)
      }

      def validatePid(s: String): Option[String] =
        Option(s).filter(s => s.length === 9 && Utilities.parseInt(s).isDefined)

      val keyValues = csv
        .split(" ")
        .toList
        .flatMap(_.split(":").toList match {
          case key :: value :: Nil => Some((key, value))
          case _ => None
        })
        .toMap
      (
        keyValues.get("byr").flatMap(validateNumeric(_, 1920, 2002)),
        keyValues.get("iyr").flatMap(validateNumeric(_, 2010, 2020)),
        keyValues.get("eyr").flatMap(validateNumeric(_, 2020, 2030)),
        keyValues.get("hgt").flatMap(validateHgt),
        keyValues.get("hcl").flatMap(validateHcl),
        keyValues.get("ecl").flatMap(validateEcl),
        keyValues.get("pid").flatMap(validatePid)
      ).mapN(Passport(_, _, _, _, _, _, _, keyValues.get("cid")))
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        lines <- Utilities
          .loadSolutionFile[IO]("/day4.txt", blocker)
          // shamelessly stolen from
          // https://medium.com/se-notes-by-alexey-novakov/scala-fs2-handle-broken-csv-lines-8d8c4defcd88
          .scan(("", "")) {
            case ((acc, _), line) =>
              if (line.isEmpty()) (line, acc) else (s"$acc $line", "")
          }
          .collect {
            case (_, line)
                if (line.trim.nonEmpty && Passport
                  .parseFromCsv(line.trim)
                  .isDefined) =>
              1
          }
          .compile
          .toList
        _ <- Console[IO].putStrLn(s"Part1: ${lines.sum}")
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

}
