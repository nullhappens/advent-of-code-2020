package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import cats.effect.Console.implicits._
import java.util.concurrent.TimeUnit
import cats.Show
import scala.collection.immutable.Nil

object Day5 extends IOApp {

  implicit val seatOrdering: Ordering[Seat] = (s1, s2) =>
    if (s1.row == s2.row)
      s1.column.compare(s2.column)
    else
      s1.row.compare(s2.row)

  final case class Seat(row: Int, column: Int) {
    val id: Int = (row * 8) + column
  }

  // object Seat {}

  final case class Range(low: Int, high: Int) {

    val midpoint: Int =
      ((high.toDouble - low.toDouble) / 2).intValue()

    def pickLowerHalf: Range =
      Range(low, midpoint + low)

    def pickUpperHalf: Range =
      Range(midpoint + low + 1, high)
  }

  def findSeat(s: String): Option[Seat] = {

    def rangeIterate(currentRange: Range, lst: List[Char]): Option[Int] = {
      lst match {
        case Nil => None
        case head :: Nil if (head === 'F' || head === 'L') =>
          currentRange.low.some
        case head :: Nil if (head === 'B' || head === 'R') =>
          currentRange.high.some
        case head :: next if (head === 'F' || head === 'L') =>
          rangeIterate(currentRange.pickLowerHalf, next)
        case head :: next if (head === 'B' || head === 'R') =>
          rangeIterate(currentRange.pickUpperHalf, next)
      }
    }

    for {
      stringList <- Option(s.toList).filterNot(_.length === 9)
      rows <- stringList.take(7).some
      columns <- stringList.drop(7).take(3).some
      row <- rangeIterate(Range(0, 127), rows)
      column <- rangeIterate(Range(0, 7), columns)
    } yield Seat(row, column)

  }

  def findMissingSeats(seats: List[Seat]): List[Seat] = {
    val colMap = (0 to 7).toList
    val seatMap = seats
      .groupBy(_.row)

    seatMap.toList
      .filter(_._2.length === 7)
      .flatMap {
        case (row, seats) =>
          colMap.diff(seats.map(_.column)).map { col =>
            Seat(row, col)
          }
      }
  }

  object Seat {
    implicit val seatShow: Show[Seat] = s =>
      s"row ${s.row}, column ${s.column}, seat ID ${s.id}"
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        seats <- Utilities
          .loadSolutionFile[IO]("/day5.txt", blocker)
          .filterNot(_.trim.isEmpty())
          .map(findSeat)
          .compile
          .toList
        missingSeats = findMissingSeats(seats.flatten)
        _ <- Console[IO].putStrLn(s"Missing seat ids ${missingSeats.map(_.id)}")
        max = seats.flatten.maxBy(_.id)
        _ <- Console[IO].putStrLn(s"Max seat id: $max ${max.id}")
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

}
