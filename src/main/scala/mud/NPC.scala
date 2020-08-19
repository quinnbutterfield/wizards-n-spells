package mud

import akka.actor.Actor
import akka.actor.ActorRef

class NPC(name: String, room: ActorRef) extends Actor {
  import NPC._
  var location = room
  val style = "npc"
  def receive = {
    case NewRoom(r) =>
      location ! Room.DropPerson(self, style)
      location ! Room.Action("left the room.")
      location = r
      location ! Room.Action("entered ")
    case m =>
      println("NPC got bad message")
  }

}
object NPC {
  case class NewRoom(r: ActorRef)
}
