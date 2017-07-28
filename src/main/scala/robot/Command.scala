package robot

import robot.Robot.SimulationState

import scala.annotation.tailrec
import scala.util.Try

sealed trait Command {
  def run: SimulationState[Unit]
}

sealed trait CommandParser {
  def parse(s: String): Option[Command]
}

sealed trait SimpleCommand extends Command with CommandParser {
  override def parse(s: String): Option[Command] =
    if (getClass.getSimpleName.stripSuffix("$").equalsIgnoreCase(s)) Some(this)
    else None
}

case class Place(p: Position) extends Command {
  override def run = Robot.place(p)
}

case object Place extends CommandParser {
  val regex = "(?i)PLACE\\s+(\\d+),\\s*(\\d+),\\s*([a-zA-Z]+)".r
  override def parse(s: String) =
    s match {
    case regex(x, y, f) =>
      Try(Place(Position(x.toInt, y.toInt, Direction.withName(f.toLowerCase.capitalize)))).toOption
    case _ => None
  }
}

case object Move extends SimpleCommand {
  override def run = Robot.move
}

case object Left extends SimpleCommand {
  override def run = Robot.left
}

case object Right extends SimpleCommand {
  override def run = Robot.right
}

case object Report extends SimpleCommand {
  override def run = Robot.report.map(println)
}

object Command {
  def parse(s: String): Option[Command] = {
    @tailrec
    def go(s: String, parsers: Seq[CommandParser]): Option[Command] =
      parsers match {
        case h :: t => h.parse(s) match { case c @ Some(_) => c; case _ => go(s, t) }
        case _      => None
      }
    go(s.trim(), Seq(Place, Move, Left, Right, Report))
  }
}
