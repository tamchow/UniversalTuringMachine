package in.tamchow.turing

import scala.language.postfixOps

/**
  * Encapsulates a single possible operation of a Turing Machine, but here I call it the "Command"
  */
object TuringCommand {
  /**
    * Match-all Wildcard
    */
  val MatchAnythingCode = "*"
  /**
    * Null (Blank) value Wildcard
    */
  val BlankCode = "!"
  /**
    * Number of arguments to [[TuringCommand]], i.e., it's arity
    */
  val CommandArity = 5
  /**
    * the regex for whtespace
    */
  val WhitespaceRegex = "\\s+"

  /**
    * Allowed wildcards:
    *
    * 1. '!' - indicates a null value or the halting command, internally represented as `null`
    *
    * 2. '*' - indicates a match-all wildcard character, no separate internal representation
    *
    * Separator used is whitespace - 5 terms are required after tokenization
    *
    * @param data `String` indicating a defined transitional operation
    * @return a [[TuringCommand]] object derived from the argument
    * @see [[MatchAnythingCode]]
    * @see [[BlankCode]]
    * @see [[CommandArity]]
    * @see [[WhitespaceRegex]]
    */
  def apply(data: String): TuringCommand = {
    val elements = escapeNull((data split(WhitespaceRegex, CommandArity)).toSeq)
    TuringCommand((elements.head, elements(1), elements(2), elements(3), MoveDirection parse elements.last))
  }

  def escapeNull(elements: Seq[String]) = elements map {
    case TuringCommand.BlankCode => null
    case others => others
  }

  /**
    * Creates a [[TuringCommand]] object from the argument
    *
    * @param data a tuple containing the necessary arguments for constructing a [[TuringCommand]] object
    * @return a [[TuringCommand]] object determined by the argument
    */
  def apply(data: (String, String, String, String, MoveDirection)): TuringCommand =
    new TuringCommand(data._1, data._2, data._3, data._4, data._5)
}

/**
  * Denotes a possible command of a [[UniversalTuringMachine]]
  *
  * @constructor
  * @param currentState the current state for which this command is applicable
  * @param nextState    the next state which this command will lead to
  * @param currentValue the current value for which this command is applicable
  * @param nextValue    the next value which this command will lead to
  * @param direction    the direction the tape head will move in after this command has executed
  */
final case class TuringCommand private(currentState: String, nextState: String, currentValue: String, nextValue: String, direction: MoveDirection) {

  import TuringCommand._

  /**
    * Checks whether the value is a match-all-values wildcard
    *
    * @return whether the value is a match-all-values wildcard
    */
  def valueMatchesEverything = currentValue == MatchAnythingCode

  /**
    * Checks whether the state is a match-all-states wildcard
    *
    * @return whether the state is a match-all-states wildcard
    */
  def stateMatchesEveryThing = currentState == MatchAnythingCode

  /**
    * The output format is deliberately similar to the input format,
    * so the result of one can be reflexively used with the other.
    *
    * @return a `String` representing this [[TuringCommand]] object
    */
  override def toString = Seq(currentState, nextState, currentValue, nextValue, direction.name) map {
    case blankItem if blankItem == UniversalTuringMachine.BlankValue => BlankCode
    case notBlankItem => notBlankItem
  } mkString " "
}