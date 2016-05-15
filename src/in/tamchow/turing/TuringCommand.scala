package in.tamchow.turing

import scala.language.postfixOps

/**
  * Encapsulates a single possible operation of a Turing Machine, but here I call it the "Command"
  */
object TuringCommand {
  val MATCH_ANYTHING_CODE = "*"
  val NULL_CODE = "!"

  def apply(currentStateP: String, nextStateP: String, currentValueP: String, nextValueP: String,
            directionP: MoveDirection.MoveDirection) =
    new TuringCommand(currentStateP, nextStateP, currentValueP, nextValueP, directionP) {}

  def apply(data: (String, String, String, String, MoveDirection.MoveDirection)) = new TuringCommand(data) {}

  def apply(data: String) = fromString(data)

  /**
    * Allowed wildcards:
    *
    * 1. '!' - indicates a null value or the halting state, internally represented as `null`
    * 2. '*' - indicates a match-all wildcard character, no separate internal representation
    *
    * Separator used is whitespace - 5 terms are required after tokenization
    *
    * @param data [[String]] indicating a defined transitional operation
    * @return a [[TuringCommand]] object derived from the argument
    * @see [[TuringCommand.MATCH_ANYTHING_CODE]]
    * @see [[TuringCommand.NULL_CODE]]
    */
  def fromString(data: String) = {
    var elements = (data split("\\s+", 5)).toList
    elements = elements map {
      case NULL_CODE => null
      case others => others
    }
    new TuringCommand((elements.head, elements(1), elements(2), elements(3), directionFromString(elements.last))) {}
  }

  private[this] def directionFromString(data: String): MoveDirection.MoveDirection = {
    try MoveDirection.fromInt(data.toInt) catch {
      case numberFormatException: NumberFormatException => try MoveDirection.withName(data) catch {
        case exception: Exception => exception.printStackTrace(); null
      }
      case _: Exception => null
    }
  }
}

abstract class TuringCommand(currentStateP: String, nextStateP: String, currentValueP: String, nextValueP: String,
                             directionP: MoveDirection.MoveDirection) {
  private[this] val _currentState = currentStateP
  private[this] val _nextState = nextStateP
  private[this] val _currentValue = currentValueP
  private[this] val _nextValue = nextValueP
  private[this] val _direction = directionP

  def this(data: (String, String, String, String, MoveDirection.MoveDirection)) {
    this(data._1, data._2, data._3, data._4, data._5)
  }

  def valueMatchesEverything() = currentValue == TuringCommand.MATCH_ANYTHING_CODE

  def stateMatchesEveryThing() = currentState == TuringCommand.MATCH_ANYTHING_CODE

  def currentState = _currentState

  override def toString = {
    this.getClass.getName + currentState + currentValue + nextState + nextValue + direction
  }

  override def equals(that: Any) =
    that match {
      case that: TuringCommand => that.isInstanceOf[TuringCommand] && this.## == that.##
      case _ => false
    }

  override def hashCode() = {
    val prime = 31
    var result: Int = 1
    result = prime * result + (if (currentState == null) 0 else currentState.##)
    result = prime * result + (if (nextState == null) 0 else nextState.##)
    result = prime * result + (if (currentValue == null) 0 else currentValue.##)
    result = prime * result + (if (nextValue == null) 0 else nextValue.##)
    result = prime * result + (if (direction == null) 0 else direction.##)
    result
  }

  def currentValue = _currentValue

  def nextState = _nextState

  def nextValue = _nextValue

  def direction = _direction
}