package in.tamchow.turing
/**
  * Indicates the directions in which the tape head can move
  */
object MoveDirection extends Enumeration {
  type MoveDirection = Value
  val LEFT, RIGHT, NONE = Value

  def fromInt(value: Int) = {
    if (value < 0) {
      LEFT
    } else if (value > 0) {
      RIGHT
    } else {
      NONE
    }
  }
}

class MoveDirection extends Enumeration {
  override def equals(that: Any) =
    that match {
      case that: MoveDirection.MoveDirection => that.isInstanceOf[MoveDirection.MoveDirection] && this.## == that.##
      case _ => false
    }

  /**
    * @throws IllegalArgumentException if object is not a valid enum type as regards [[MoveDirection.MoveDirection]]
    * @return the hash code of this [[MoveDirection.MoveDirection]] object
    */
  override def hashCode() = {
    import MoveDirection._
    this match {
      case LEFT => -1
      case RIGHT => +1
      case NONE => 0
      case other => throw new IllegalArgumentException("Illegal data in enum of type " + other.getClass)
    }
  }
}