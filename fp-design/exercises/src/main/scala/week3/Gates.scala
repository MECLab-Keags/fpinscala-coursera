package week3

class Inverter extends Simulation {
  val InverterDelay: Int = 50

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSig }
    }

    input addAction invertAction
  }
}

