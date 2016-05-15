package in.tamchow.turing

/**
  * Indicates the directions in which the tape head can move
  */
object MoveDirection {
  val illegalTypeMessage = s"Unrecognized enum identifier : %s for type $getClass"
  val LEFT = MoveDirection(-1, "LEFT")
  val RIGHT = MoveDirection(+1, "RIGHT")
  val NONE = MoveDirection(0, "NONE")

  def parse(data: String) = {
    try MoveDirection(data.toInt) catch {
      case numberFormatException: NumberFormatException => try MoveDirection(data) catch {
        case exception: Exception => exception.printStackTrace(); null
      }
      case _: Exception => null
    }
  }

  def apply(value: Int): MoveDirection = {
    value match {
      case positive if value > 0 => RIGHT
      case negative if value < 0 => LEFT
      case zero => NONE
    }
  }

  def apply(name: String): MoveDirection = {
    name match {
      case left if name equalsIgnoreCase LEFT.name => LEFT
      case right if name equalsIgnoreCase RIGHT.name => RIGHT
      case none if name equalsIgnoreCase NONE.name => NONE
      case other => throw new IllegalArgumentException(illegalTypeMessage format other)
    }
  }
}

case class MoveDirection(value: Int, _name: String) {
  def name = _name
}