package robot

import cats.data.State

import scala.annotation.tailrec

sealed abstract case class Direction(left: Direction, right: Direction) {
  @tailrec
  final def rotate(units: Int): Direction =
    units match {
      case i if i > 0 => right.rotate(units - 1)
      case i if i < 0 => left.rotate(units + 1)
      case _          => this
    }
}
case object North extends Direction(West, East)
case object South extends Direction(East, West)
case object East  extends Direction(North, South)
case object West  extends Direction(South, North)

case class Robot(position: Option[Position])

case class Position(x: Int, y: Int, facing: Direction) {
  def next: Position = facing match {
    case North => copy(y = y + 1)
    case South => copy(y = y - 1)
    case East  => copy(x = x + 1)
    case West  => copy(x = x - 1)
  }
}

case class Table(width: Int, height: Int) {
  def valid(p: Position): Boolean =
    (0 until width contains p.x) && (0 until height contains p.y)
}

case class Simulation(robot: Robot, table: Table)

object Robot {
  type SimulationState[A] = State[Simulation, A]

  def apply(p: Position): Robot = Robot(Some(p))

  def place(p: Position): SimulationState[Unit] =
    State.modify { s =>
      if (s.table.valid(p)) s.copy(robot = Robot(p))
      else s
    }

  def move: SimulationState[Unit] =
    State.modify {
      case s @ Simulation(Robot(Some(p)), t) =>
        if (t.valid(p.next)) s.copy(robot = Robot(p.next))
        else s
      case s => s
    }

  def right: SimulationState[Unit] = rotate(1)

  def left: SimulationState[Unit] = rotate(-1)

  def rotate(units: Int): SimulationState[Unit] =
    State.modify {
      case s @ Simulation(Robot(Some(p)), _) =>
        s.copy(robot = Robot(p.copy(facing = p.facing.rotate(units))))
      case s => s
    }

  def report: SimulationState[String] =
    State.get[Simulation].map {
      case s @ Simulation(Robot(Some(p)), _) =>
        s"${p.x}, ${p.y}, ${p.facing}"
      case _ => "Robot has not been placed"
    }
}
