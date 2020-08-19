package mud

import akka.actor.ActorSystem


import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import java.io.BufferedReader
import java.io.PrintStream
import java.net.Socket

class PlayerSupervisor extends Actor {
  import PlayerSupervisor._
  def receive = {
    case NewPlayer(inventory, sock, in, out, name) =>
      for(child <- context.children) child ! Player.PrintMessage("\033[36m" + name + " connected!")
      val p = context.actorOf(Props(new Player(inventory, sock, in, out, name)), name)
      Main.roomSuper ! RoomSupervisor.StartPlayer(p)
    case Tell(user, msg) =>
      for (child <- context.children if(child.path.name == user)) child ! Player.PrintMessage("\033[36m" + sender.path.name + " whispers " + msg)
    case ProcessInput =>
      for (child <- context.children) child ! Player.ProcessInput
    case m => println("PlayerSupervisor got bad message: " + m)
  }

}
object PlayerSupervisor {
  case object ProcessInput
  case object GetRooms
  case class Tell(user:String, msg:String)
  case class NewPlayer(inventory: List[Item], sock: Socket, in: BufferedReader, out: PrintStream, name: String)
}