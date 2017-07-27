package robot

import java.lang.Math.floorMod

import cats.data.State

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value

  def rotate(d: Direction, units: Int): Direction = {
    val vs = values.toList
    vs(floorMod(vs.indexOf(d) + units, vs.length))
  }
}
import robot.Direction._

case class Robot(position: Position)

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

case class Simulation(robot: Option[Robot], table: Table) {
  def place(r: Robot): Simulation = copy(robot = Some(r))
}

object Robot {
  type SimulationState[A] = State[Simulation, A]

  def place(p: Position): SimulationState[Unit] =
    State.modify { s =>
      if (s.table.valid(p)) s.place(Robot(p))
      else s
    }

  def move: SimulationState[Unit] =
    State.modify {
      case s @ Simulation(Some(Robot(p)), t) =>
        if (t.valid(p.next)) s.place(Robot(p.next))
        else s
      case s => s
    }

  def right: SimulationState[Unit] = rotate(1)

  def left: SimulationState[Unit] = rotate(-1)

  def rotate(units: Int): SimulationState[Unit] =
    State.modify {
      case s @ Simulation(Some(Robot(p)), _) =>
        s.place(Robot(p.copy(facing = Direction.rotate(p.facing, units))))
      case s => s
    }

  def report: SimulationState[String] =
    State.get[Simulation].map {
      case s @ Simulation(Some(Robot(p)), _) =>
        s"${p.x}, ${p.y}, ${p.facing}"
      case _ => "Robot has not been placed"
    }
}
