package com.nullhappens.aoc

import java.util.concurrent.TimeUnit

import cats.Show
import cats.effect.Console.implicits._
import cats.effect._
import cats.implicits._

import com.nullhappens.aoc.Day8.Command.{Acc, Jmp, Nop}

object Day8 extends IOApp {

  implicit val throwableShow: Show[Throwable] = Show.fromToString

  sealed trait Command
  object Command {
    implicit val commandShow: Show[Command] = Show.fromToString
    final case object Nop extends Command
    final case object Acc extends Command
    final case object Jmp extends Command

    def parseFromString(s: String): Option[Command] =
      s.toLowerCase().trim() match {
        case "nop" => Some(Nop)
        case "acc" => Some(Acc)
        case "jmp" => Some(Jmp)
        case _ => None
      }
  }

  final case class Instruction(command: Command, op: Int)

  object Instruction {
    implicit val instructionShow: Show[Instruction] = Show.fromToString

    def parseFromString(s: String): Option[Instruction] = {
      val instruction = "^\\s*(nop|jmp|acc)\\s*((?:\\+|\\-)\\d*)\\s*$".r
      s match {
        case instruction(command, number) =>
          for {
            n <- Utilities.parseInt(number)
            c <- Command.parseFromString(command)
          } yield Instruction(c, n)
        case _ => None
      }
    }
  }

  type Program = Vector[Instruction]

  def runProgram(program: Program): IO[Long] = {

    def loop(acc: Long, pc: Long, pcHistory: Set[Long]): IO[Long] = {
      if (pcHistory.contains(pc))
        IO.raiseError(
          new RuntimeException(
            s"infinite loop.  Acc at $acc"
          )
        )

      //TODO: is this stop condition correct?
      else if (pc >= program.length)
        acc.pure[IO]
      else {
        program.get(pc) match {
          case Some(instruction) =>
            instruction.command match {
              case Nop =>
                loop(acc, pc + 1, pcHistory + pc)
              case Acc =>
                val newAcc = acc + instruction.op
                loop(newAcc, pc + 1, pcHistory + pc)
              case Jmp =>
                val newPc = pc + instruction.op
                if (newPc < 0 || newPc >= program.length)
                  Console[IO].putError(
                    s"Jmp exceeds program bounds instruction: $instruction pc: $pc"
                  ) *>
                  acc.pure[IO]
                else
                  loop(acc, newPc, pcHistory + pc)

            }
          case None =>
            IO.raiseError(
              new IndexOutOfBoundsException(
                s"Jmp exceeds program bounds instruction pc: $pc"
              )
            )
        }
      }
    }
    loop(0, 0, Set.empty[Long])
  }

  def fixProgram(program: Program): IO[Vector[Either[Throwable, Long]]] = {
    def flipInstruction(instruction: Instruction): Instruction =
      instruction.command match {
        case Nop => Instruction(Jmp, instruction.op)
        case Acc => instruction
        case Jmp => Instruction(Nop, instruction.op)
      }
    def replaceInstruction(
        instruction: Instruction,
        idx: Int,
        toReplace: Program
      ): Program =
      (toReplace.take(idx) :+ flipInstruction(instruction)) ++
        toReplace.drop(idx + 1)

    val replacements = program.zipWithIndex
      .filter {
        case (inst, _) =>
          (inst.command.isInstanceOf[Jmp.type] || inst.command
            .isInstanceOf[Nop.type])
      }

    replacements
      .traverse {
        case (instruction, idx) =>
          val newProgram = replaceInstruction(instruction, idx, program)
          runProgram(newProgram).attempt
      }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { blocker =>
      for {
        start <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        program <- Utilities
          .loadSolutionFile[IO]("/day8.txt", blocker)
          .filter(_.nonEmpty)
          .evalMap(s =>
            Instruction
              .parseFromString(s)
              .fold(
                IO.raiseError[Instruction](
                  new RuntimeException(s"parse error at $s")
                )
              )(_.pure[IO])
          )
          .compile
          .toVector
        errors <- runProgram(program).attempt
        _ <- errors.fold(
          err => Console[IO].putError(err),
          r => Console[IO].putStrLn(s"Result of program $r")
        )
        _ <- Console[IO].putStrLn("Part 2")
        result <- fixProgram(program)
        _ <- result.filter(_.isRight).traverse(Console[IO].putStrLn(_))
        end <- Timer[IO].clock.realTime(TimeUnit.MILLISECONDS)
        _ <- Console[IO].putStrLn(s"Run time: ${end - start}ms")
      } yield (ExitCode.Success)
    }
  }

}
