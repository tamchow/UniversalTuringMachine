package in.tamchow.turing

/**
  * Indicates the directions in which the tape head can move
  */
object MoveDirection {
  val illegalTypeMessage = s"Unrecognized enum identifier : %s for type $getClass"
  val (left, right, none) = (MoveDirection(-1, "left"), MoveDirection(+1, "right"), MoveDirection(0, "none"))

  /**
    * Results:
    *
    * 1. Any negative [[Int]] value or case-insensitive match to [[left.name]]-> [[left]]
    *
    * 2. Any positive [[Int]] value or case-insensitive match to [[right.name]]-> [[right]]
    *
    * 3. A Zero (`0`) [[Int]] value or case-insensitive match to [[none.name]]-> [[none]]
    *
    * @param data the [[String]] to parse into a [[MoveDirection]]
    * @return a [[MoveDirection]] object based on the parameters
    */
  def parse(data: String) = {
    def handleOtherExceptions(exception: Exception) = {
      exception printStackTrace()
      null
    }
    try MoveDirection(data.toInt) catch {
      case numberFormatException: NumberFormatException => try MoveDirection(data) catch {
        case exception: Exception => handleOtherExceptions(exception)
      }
      case exception: Exception => handleOtherExceptions(exception)
    }
  }

  def apply(value: Int): MoveDirection = {
    value match {
      case positive if value > 0 => right
      case negative if value < 0 => left
      case zero => none
    }
  }

  def apply(name: String): MoveDirection = {
    def verifyName(verifier: MoveDirection) =
    //in order of preference : fully-qualified names are preferred over the first letter match
      (name equalsIgnoreCase verifier.name) || (name(0).toString equalsIgnoreCase verifier.name(0).toString)
    name match {
      case isLeft if verifyName(left) => left
      case isRight if verifyName(right) => right
      case isNone if verifyName(none) => none
      case other => throw new IllegalArgumentException(illegalTypeMessage format other)
    }
  }
}

sealed case class MoveDirection private(value: Int, name: String)