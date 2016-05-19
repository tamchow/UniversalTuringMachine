package in.tamchow.turing

import scala.language.postfixOps

/**
  * The Launcher for the Universal Turing Machine simulator
  *
  * The parameters are, in order:
  *
  * 1. The input file path
  *
  * 2. The tape size
  *
  * 3. The number of steps for which to run the program. Leave it negative to run it till it halts.
  *
  * 4. The number of milliseconds to pause before starting the next step.
  *
  * Leave this negative to pause for user input before every step.
  *
  * All these parameters are required.
  *
  * Only supports program input as files.
  */
object TuringMain {

  private val pausedMessage = "Execution Paused - Press any key to continue."
  private val noParametersSpecifiedMessage = "No parameters specified"

  def main(args: Array[String]) {
    require(args.length <= 0, noParametersSpecifiedMessage)
    doRunMain(args(0), args(1).toInt, args(2).toInt, args(3).toInt)
  }

  def doRunMain(filePath: String, tapeSize: Int, steps: Int, pauseTime: Int) {
    val data = io.Source fromFile filePath getLines() toVector
    val (turingMachine, useStepping) = (UniversalTuringMachine(data, tapeSize), steps >= 0)
    def doRun(halt: Boolean, currentSteps: Int, head: Int, state: String, tape: Vector[String]) {
      if (halt || (useStepping && currentSteps >= steps)) println(formatArgs(tape))
      else {
        val (continue, nextHead, nextState, nextTape) = turingMachine runStep(head, state, tape)
        if (pauseTime >= 0) Thread sleep pauseTime
        else {
          println(pausedMessage)
          io.StdIn readLine()
        }
        doRun(!continue, currentSteps + 1, nextHead, nextState, nextTape)
      }
    }
    doRun(halt = false, 0, 0, turingMachine initialState, turingMachine initTape)
  }

  def formatArgs(args: Seq[String]) = args map {
    case null => ""
    case others => others
  } mkString " "
}