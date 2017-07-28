package robot

import cats.data.State
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import fs2.{io, text}

object Main extends App with LazyLogging {

  val initialState = Simulation(None, Table(5, 5))

  io.stdin[IO](32)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(Command.parse)
    .collect { case Some(cmd) => cmd }
    .runFold(State.pure[Simulation, Unit](())) { (z, cmd) => z.flatMap(_ => cmd.run) }
    .unsafeRunSync().runA(initialState)
}
