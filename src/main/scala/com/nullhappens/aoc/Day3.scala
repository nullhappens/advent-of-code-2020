package com.nullhappens.aoc

import cats.effect._
import cats.implicits._
import cats.effect.Console.implicits._
import cats.Show
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

sealed trait MapBlock

object MapBlock {
  implicit val mapBlockShow: Show[MapBlock] = _ match {
    case x: Tree => x.show
    case x: Empty => x.show
    case x: Invalid => x.show
  }
  implicit val treeShow: Show[Tree] = _ => "#"
  implicit val emptyShow: Show[Empty] = _ => "."
  implicit val invalidShow: Show[Invalid] = _ => "X"

  final case class Tree() extends MapBlock
  final case class Empty() extends MapBlock
  final case class Invalid() extends MapBlock
}

final case class Slope(x: Int, y: Int)

object Day3 extends IOApp {
  import MapBlock._

  type Matrix = List[List[MapBlock]]

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        lines <- Utilities
          .loadSolutionFile[IO]("/day3.txt", blocker)
          .compile
          .toList
        matrix <- IO(transform(lines))
        _ <- IO.raiseUnless(validateMatrix(matrix))(
          new RuntimeException("Map has invalid spaces")
        )
        treeCount1 <- IO(walk(Slope(3, 1), matrix, 0, 0))
        treeCount2 <- IO(walk(Slope(1, 1), matrix, 0, 0))
        treeCount3 <- IO(walk(Slope(5, 1), matrix, 0, 0))
        treeCount4 <- IO(walk(Slope(7, 1), matrix, 0, 0))
        treeCount5 <- IO(walk(Slope(1, 2), matrix, 0, 0))
        _ <- Console[IO].putStrLn(s"Part1: $treeCount1")
        _ <- Console[IO].putStrLn(
          s"$treeCount2 ,$treeCount1, $treeCount3, $treeCount4, $treeCount5"
        )
        mult <- IO.fromOption(
          for {
            x1 <- treeCount1
            x2 <- treeCount2
            x3 <- treeCount3
            x4 <- treeCount4
            x5 <- treeCount5
          } yield (x1.toLong * x2.toLong * x3.toLong * x4.toLong * x5.toLong)
        )(new RuntimeException("Invalid"))
        _ <- Console[IO].putStrLn(s"Part2: $mult")
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

  def validateMatrix(mx: Matrix): Boolean =
    mx.forall(lst => !lst.exists(_.isInstanceOf[Invalid]))

  def walk(slope: Slope, mx: Matrix, startX: Int, startY: Int): Option[Int] = {

    def nextX(currentX: Int, rowSize: Int): Int = (currentX + slope.x) % rowSize

    def nextY(currentY: Int): Int = currentY + slope.y

    @tailrec
    def loop(x: Int, y: Int, treeCount: Int): Option[Int] =
      if (y < mx.length) {
        val row = mx(y)
        row(x) match {
          case Tree() => loop(nextX(x, row.length), nextY(y), treeCount + 1)
          case Empty() => loop(nextX(x, row.length), nextY(y), treeCount)
          case Invalid() => None
        }
      } else Some(treeCount)
    loop(startX, startY, 0)
  }

  def transform(lst: List[String]): Matrix =
    lst.map { s =>
      s.split("").toList.map {
        case x if x === "." => Empty()
        case x if x === "#" => Tree()
        case _ => Invalid()
      }
    }
}
