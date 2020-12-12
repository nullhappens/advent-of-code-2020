package com.nullhappens.aoc

import scala.collection.immutable.Nil

import cats.Show
import cats.effect._
import cats.implicits._

object Day7 extends IOApp {

  final case class CanContain(color: String, count: Int)
  object CanContain {
    implicit val canContainShow: Show[CanContain] = Show.fromToString
  }

  // final case class BagRule(from: Bag, to: Option[Bag], quantity: Int)
  // object BagRule {
  //   implicit val bagRuleShow: Show[BagRule] = Show.fromToString
  // }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        lines <- Utilities
          .loadSolutionFile[IO]("/day7sample.txt", blocker)
          .filter(_.nonEmpty)
          .compile
          .toList
        // _ <- lines.traverse(Console[IO].putStrLn)
        rules = lines.map(transform).toMap
        // _ <- Console[IO].putStrLn(
        //   s"loaded file equal to parsed file? ${lines.toSet.eqv(rulesToString(rules).toSet)}"
        // )
        // _ <- Console[IO].putStrLn(lines)
        // _ <- Console[IO].putStrLn(rulesToString(rules))
        // _ <- Console[IO].putStrLn(s"loaded ${rules.length} rules")
        // _ <- rules.traverse(x => Console[IO].putStrLn(x))
        // _ <- Console[IO].putStrLn(
        //   s"Part 1: ${solutionPart1(rules, "shiny gold")}"
        // )
        // _ <- rules.traverse(br => Console[IO].putStrLn(br))
        // _ <- Console[IO].putStrLn(findColorPermutations(rules, "shiny gold"))
        // _ <- Console[IO].putStrLn(rules)
        // _ <- Console[IO].putStrLn(solutionPart1Text(rules, Bag("shiny gold")))
      } yield (ExitCode.Success)
    }
  }

  //     rules: Map[String, List[CanContain]],
  //     color: String
  //   ): Int = {
  //
  //   def containsColor(lst: List[CanContain]): Int =
  //     lst.filter(_.color === color).count
  //
  //   rules.flatMap{ case (_, containList) =>
  //
  //   }
  // }

  // rules.get(color) match {
  //   case Some(lst) => lst.map(_.color)
  //   case None => 0
  // }

  // def solutionPart1Text(bagRules: List[BagRule], targetBag: Bag): Unit = {
  //   val collect = bagRules.filter(_.to.contains(targetBag))
  //   collect.foreach(b =>
  //     println(s"A ${b.from.color} bag, which can hold ${b.to.map(_.color)} bag")
  //   )
  //
  //   // val needToFindThese = collect.map(_.from)
  //
  //   val foundMoreShit =
  //     collect.filter(r => bagRules.filter(b => b.to.contains(r.from)).nonEmpty)
  //
  //   foundMoreShit.foreach(b =>
  //     println(s"A ${b.from.color} bag, which can hold ${b.to.map(_.color)} bag")
  //   )
  //
  // }
  // def solutionPart1(bagRules: List[BagRule], targetBag: Bag): Int = {
  //
  //   def loop(acc: Int, targetBags: List[Bag]): Int = {
  //     // println(s"acc: $acc ${targetBags}")
  //     targetBags match {
  //       case Nil => acc
  //       case head :: next =>
  //         // println(
  //         //   s"\t ${bagRules.collect { case b if b.to.contains(head) => b.from }}"
  //         // )
  //         val moreBags = bagRules.collect {
  //           case b if b.to.contains(head) => b.from
  //         }
  //         bagRules.foreach { r =>
  //           println(s"A ${r.to} bag, which can hold $head")
  //         }
  //
  //         if (moreBags.isEmpty) {
  //           // println(s"end of the road for $head")
  //           loop(acc, next)
  //         } else {
  //           loop(acc + 1, moreBags ::: next)
  //         }
  //       // loop(acc + 1, bagRules.collect {
  //       //   case b if b.to.contains(head) => b.from
  //       // } ::: next)
  //     }
  //   }
  //   val start = bagRules.collect {
  //     case b if b.to.contains(targetBag) =>
  //       println(
  //         s"A ${b.from.color} bag, which can hold ${targetBag.color} bag directly"
  //       )
  //       b.from
  //   }
  //   loop(start.length, start)
  //
  //   // def loop(acc: Int, bagRules: List[BagRules], )
  //
  //   // val directMatches = bagRules.filter(_.to.contains(targetBag))
  //   // println("direct matches: ")
  //   // directMatches.foreach(println)
  //   //
  //   // val indirectMatches =
  //   //   directMatches
  //   //     .map(_.from)
  //   //     .flatMap(b => bagRules.filter(_.to.contains(b)))
  //   //     .distinctBy(_.from)
  //   // println("indirect matches")
  //   // indirectMatches.foreach(println)
  //   // directMatches.length + indirectMatches.length
  // }

  // def rulesToString(rules: List[BagRule]): List[String] =
  //   rules
  //     .groupBy(_.from)
  //     .toList
  //     .map {
  //       case (bag, bags) =>
  //         s"${bag.color} bags contain".concat(
  //           bags
  //             .map { br =>
  //               val quantityStr = Option(br.quantity)
  //                 .collect { case x if x > 1 => "bags" }
  //                 .getOrElse("bag")
  //               br.to match {
  //                 case Some(bag) => s"${br.quantity} ${bag.color} $quantityStr"
  //                 case None => "no other bags"
  //               }
  //             }
  //             .mkString(" ", ", ", ".")
  //         )
  //     }
  // .mkString("\n")

  def transform(input: String): (String, List[CanContain]) = {

    def getRules(rules: String): List[CanContain] = {
      // println(s"\t\t getting rules for $s")
      rules
        .split(",")
        .toList
        .flatMap { rule =>
          // println(s"\t\t\t transforming $rule")
          val regex = "^(\\d)*".r
          val quantity =
            regex
              .findFirstIn(rule.trim())
              .flatMap(Utilities.parseInt)
          val color = regex.replaceAllIn(rule.trim(), "").trim()
          quantity match {
            case Some(value) => List(CanContain(color, value))
            case None => List.empty
          }
        }
    }

    input
      .replaceAll("bag(s){0,1}(\\.){0,1}\\s{0,1}", "")
      .split("contain")
      .map(_.trim)
      .toList match {
      case color :: rules :: Nil =>
        (color -> getRules(rules))
      case _ => throw new RuntimeException("unable to parse shit")
    }
  }

}
