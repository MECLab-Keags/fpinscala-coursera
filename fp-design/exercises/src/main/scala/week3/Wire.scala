package week3

class Wire {
  type Action = () => Unit

  private var sigVal = false
  private var actions: List[Action] = List()

  def getSignal: Boolean = sigVal
  def setSignal(sig: Boolean): Unit =
    if (sig != sigVal){
      sigVal = sig
      actions foreach(_())
    }

  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }
}
