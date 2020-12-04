package com.nullhappens.aoc

import cats.effect._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger

object HelloWorld extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    implicit def unsafeLogger = Slf4jLogger.getLogger[IO]
    Logger[IO].debug("Hello world!").as(ExitCode.Success)
  }

}
