package in.tamchow.turing

import scala.language.postfixOps

/**
  * Encapsulates a single possible operation of a Turing Machine, but here I call it the "Command"
  */
object TuringCommand {
  val (matchAnythingCode, nullCode, commandArity, whitespaceRegex) = ("*", "!", 5, "\\s+")

  /**
    * Allowed wildcards:
    *
    * 1. '!' - indicates a null value or the halting command, internally represented as `null`
    *
    * 2. '*' - indicates a match-all wildcard character, no separate internal representation
    *
    * Separator used is whitespace - 5 terms are required after tokenization
    *
    * @param data [[java.lang.String]] indicating a defined transitional operation
    * @return a [[TuringCommand]] object derived from the argument
    * @see [[matchAnythingCode]]
    * @see [[nullCode]]
    * @see [[commandArity]]
    * @see [[whitespaceRegex]]
    */
  def apply(data: String): TuringCommand = {
    val elements = escapeNull((data split(whitespaceRegex, commandArity)).toVector)
    TuringCommand((elements.head, elements(1), elements(2), elements(3), MoveDirection parse elements.last))
  }

  def escapeNull(elements: Vector[String]) = elements map {
    case TuringCommand.nullCode => null
    case others => others
  }

  def apply(data: (String, String, String, String, MoveDirection)): TuringCommand =
    new TuringCommand(data._1, data._2, data._3, data._4, data._5)
}

final case class TuringCommand private(currentState: String, nextState: String, currentValue: String, nextValue: String, direction: MoveDirection) {

  import TuringCommand._

  def valueMatchesEverything = currentValue == matchAnythingCode

  def stateMatchesEveryThing = currentState == matchAnythingCode

  /**
    * The output format is deliberately similar to the input format,
    * so the result of one can be reflexively used with the other.
    *
    * @return a [[java.lang.String]] representing this [[TuringCommand]] object
    */
  override def toString = Vector(currentState, nextState, currentValue, nextValue, direction.name) map {
    case nullItem if nullItem == null => nullCode
    case notNullItem => notNullItem
  } mkString " "
}