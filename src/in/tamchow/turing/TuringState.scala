package in.tamchow.turing

/**
  * Encapsulates a single possible 5-tuple operation of a Turing Machine, but here I call it the state
  */
object TuringState {
  val MATCH_ANYTHING_CODE = "*"
  val NULL_CODE = "!"

  /**
    * Allowed wildcards:
    *
    * 1. '!' - indicates a null value or the halting state, internally represented as `null`
    * 2. '*' - indicates a match-all wildcard character, no separate internal representation
    *
    * @param data [[String]] indicating a defined transitional operation
    * @return a [[TuringState]] object derived from the argument
    * @see [[TuringState.MATCH_ANYTHING_CODE]]
    * @see [[TuringState.NULL_CODE]]
    */
  private def directionFromString(data: String): MoveDirection.MoveDirection = {
    try MoveDirection.fromInt(data.toInt) catch {
      case numberFormatException: NumberFormatException => try MoveDirection.withName(data) catch {
        case exception: Exception => exception.printStackTrace(); null
      }
      case _ => null
    }
  }

  def fromString(data: String): TuringState = {
    var elements = (data split("\\s+", 5)).toList
    elements = elements map {
      case NULL_CODE => null
      case others => others
    }
    new TuringState(elements(0), elements(1), elements(2), elements(3), directionFromString(elements(4)))
  }
}

class TuringState(currentStateP: String, nextStateP: String, currentValueP: String, nextValueP: String,
                  directionP: MoveDirection.MoveDirection) {
  def valueMatchesEverything(): Boolean = currentValue == TuringState.MATCH_ANYTHING_CODE

  def stateMatchesEveryThing(): Boolean = currentState == TuringState.MATCH_ANYTHING_CODE

  val currentState = currentStateP
  val nextState = nextStateP
  val currentValue = currentValueP
  val nextValue = nextValueP
  val direction = directionP

  override def toString = {
    this.getClass.getName + currentState + currentValue + nextState + nextValue + direction
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: TuringState => that.isInstanceOf[TuringState] && this.## == that.##
      case _ => false
    }

  override def hashCode(): Int = {
    val prime = 31
    var result: Int = 1
    result = prime * result + (if (currentState == null) 0 else currentState.##)
    result = prime * result + (if (nextState == null) 0 else nextState.##)
    result = prime * result + (if (currentValue == null) 0 else currentValue.##)
    result = prime * result + (if (nextValue == null) 0 else nextValue.##)
    result = prime * result + (if (direction == null) 0 else direction.##)
    result
  }
}