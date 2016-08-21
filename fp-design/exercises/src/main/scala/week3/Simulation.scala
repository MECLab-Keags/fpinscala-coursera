package week3

abstract class Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)

  private var currTime = 0
  def currentTime: Int = currTime

  private var agenda: List[Event] = List()
  private def insert(evt: List[Event], item: Event): List[Event] = evt match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: evt
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }

  }
}
