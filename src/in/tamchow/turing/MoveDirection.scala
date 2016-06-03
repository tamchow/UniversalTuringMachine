package in.tamchow.turing

/**
  * Indicates the directions in which the tape head can move:
  *
  * 1. [[MoveDirection.Left]] -> Left (head index - 1)
  *
  * 2. [[MoveDirection.Right]] -> Right (head index + 1)
  *
  * 3. [[MoveDirection.Neither]] -> Neither left nor right (head index +/- 0 = head index)
  */
object MoveDirection {
  /**
    * Exception message for illegal type identifier
    */
  val IllegalTypeMessage = s"Unrecognized enum identifier : %s for type $getClass"
  /**
    * Indicates tape head moving left (head index - 1)
    */
  val Left = MoveDirection(-1, "Left")
  /**
    * Indicates tape head moving right (head index - 1)
    */
  val Right = MoveDirection(+1, "Right")
  /**
    * Indicates tape head moving Neither left nor right (head index +/- 0 = head index)
    */
  val Neither = MoveDirection(0, "Neither")

  /**
    * Results:
    *
    * 1. Any negative [[scala.Int]] value or case-insensitive match to [[MoveDirection.Left.name]]-> [[MoveDirection.Left]]
    *
    * 2. Any positive [[scala.Int]] value or case-insensitive match to [[MoveDirection.Right.name]]-> [[MoveDirection.Right]]
    *
    * 3. A Zero (`0`) [[scala.Int]] value or case-insensitive match to [[MoveDirection.Neither.name]]-> [[MoveDirection.Neither]]
    *
    * @param data the [[java.lang.String]] to parse into a [[MoveDirection]]
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

  /**
    * Creates a  [[MoveDirection]] object from an [[scala.Int]]
    *
    * @param value the argument determining the [[MoveDirection]] which is returned
    * @return a [[MoveDirection]] object
    */
  def apply(value: Int): MoveDirection = {
    value match {
      case positive if value > 0 => Right
      case negative if value < 0 => Left
      case zero => Neither
    }
  }

  /**
    * Creates a  [[MoveDirection]] object from a [[java.lang.String]]
    *
    * @param name the argument determining the [[MoveDirection]] which is returned
    * @return a [[MoveDirection]] object
    */
  def apply(name: String): MoveDirection = {
    def verifyName(verifier: MoveDirection) =
    //in order of preference : fully-qualified names are preferred over the first letter match
      (name equalsIgnoreCase verifier.name) || (name(0).toString equalsIgnoreCase verifier.name(0).toString)
    name match {
      case isLeft if verifyName(Left) => Left
      case isRight if verifyName(Right) => Right
      case isNone if verifyName(Neither) => Neither
      case other => throw new IllegalArgumentException(IllegalTypeMessage format other)
    }
  }
}

/**
  * Represents a direction in which the tape head can move
  *
  * @param value The distance to move in a direction
  * @param name  The name of the direction of movement
  */
final case class MoveDirection private(value: Int, name: String)