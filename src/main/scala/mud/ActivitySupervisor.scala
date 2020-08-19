package mud

import akka.actor.Actor
import akka.actor.ActorRef

class ActivitySupervisor extends Actor {
  import ActivitySupervisor._
  var counter = 0
  var dead:ActorRef = null
  // Linked List priority queue
  private val pq = new MyPriorityQueue[Activity]((t1, t2) => t1.time < t2.time)
  // Binary heap priority queue
  private val hpq = new HeapPQ[Activity]((t1, t2) => t1.time < t2.time)
  def receive = {
    case ScheduleActivity(time, actor, message) =>
      hpq.enqueue(new Activity(time, actor, message))
    case Dead(a) =>
      dead = a
    case CheckPQ =>
      counter += 1
      if (!hpq.isEmpty) {
        if (hpq.peek.time <= counter) {
          val next = hpq.dequeue()
          if(dead != next.actor){
            next.actor ! next.message
          }
          dead = null
          counter = 0
        }
      }
  }

}
object ActivitySupervisor {
  case class Activity(time: Int, actor: ActorRef, message: Any)
  case class ScheduleActivity(delay: Int, actor: ActorRef, message: Any)
  case object CheckPQ
  case class Dead(a: ActorRef)
}
