package mud

import akka.actor.Actor
import scala.util.Random
import akka.actor.ActorRef

class Room(name: String, k: String, desc: String, exitNames: Array[String], private var items: List[Item], var npcList: String) extends Actor {
  private var exits: Array[Option[ActorRef]] = null
  private var players = List[ActorRef]()
  private var NPCs = List[ActorRef]()
  import Room._
  def receive = {
    case InitNPC =>
      if (!npcList.isEmpty) {
        for (npc <- npcList.split(" ")) Main.roomSuper ! RoomSupervisor.newNPC(npc, k, self)
      }
    case MoveNPC =>
      if(!NPCs.isEmpty){
        val mover = Random.shuffle(NPCs).head
        val validExits = exits.flatten
        val newHome = Random.shuffle(validExits.toList).head
        NPCs = NPCs.filter(_ != mover)
        mover ! NPC.NewRoom(newHome)
        newHome ! AddNPC(mover)
      }
    case DropItem(item) =>
      dropItem(item)
    case GetItem(item) =>
      val foundItem: Option[Item] = getItem(item)
      sender ! Player.GotItem(foundItem)
    case LinkExits(rooms) =>
      exits = exitNames.map(rooms.get)
      sender ! RoomSupervisor.NewRoomData(self, exits)
    case PrintDescription =>
      sender ! Player.PrintMessage(description(sender))
    case GetExit(dir) =>
      sender ! Player.TakeExit(getExit(dir))
    //sender ! Player.NotInCombat
    case FindPlayer(p) =>
      
      players.find(j => j.path.name == p) match {
        case Some(i) =>
          sender ! Player.InRoom(i)
        case None =>
          sender ! Player.PrintMessage("That person isn't here.")
          sender ! Player.NotInCombat
      }
    case AddPlayer(p) =>
      players = p :: players
    case AddNPC(n) =>
      NPCs = n :: NPCs
    case DropPerson(pl, style) => {
      style match{
      case "player" =>
        players = players.filter(f => f != pl)
      case "npc" =>
        NPCs = NPCs.filter(f => f != pl)
        
      }
    } 
      
    case Say(m, p) =>
      for (player <- players) if (player != sender) player ! Player.PrintMessage("\033[36m" + p.path.name + " said " + m)
    case Action(a) =>
      if (a == "entered ") players.foreach(p => if (p != sender)
        p ! Player.PrintMessage(sender.path.name + " entered the room"))
      else {
        for (player <- players) {
          if (player != sender) player ! Player.PrintMessage(sender.path.name + " " + a)
          else player ! Player.PrintMessage("You " + a)
        }
      }

    case m => println("Room got bad message: " + m)
  }
  def description(sender: ActorRef): String = {
    "\033[33m" +
      name + "\n" + desc + "\n" + "Exits: " + convertExits.mkString(", ") + "\n" + "Items: " + showItems +
      "\n" + "Players: " + showOthers(sender) + "\n" + "NPCs: " + NPCs.map(_.path.name).mkString(", ")
  }
  def convertExits(): Array[String] = {
    for (e <- exits.filter(_ != None)) yield exits.indexOf(e) match {
      case 0 => "north"
      case 1 => "south"
      case 2 => "east"
      case 3 => "west"
      case 4 => "up"
      case 5 => "down"
    }
  }
  def showOthers(p: ActorRef): String = {
    (for (player <- players if (player != p)) yield (player.path.name)).mkString(", ")

  }
  def showItems(): String = {
    if (items.isEmpty) "None"
    else items.map(_.name).mkString(", ")
  }
  def getExit(dir: Int): Option[ActorRef] = {
    if (exits(dir) != "-") exits(dir)
    else None
  }
  def getItem(itemName: String): Option[Item] = {
    val itemNames = items.map(_.name)
    if (itemNames.contains(itemName)) {
      val itemLocation = itemNames.indexOf(itemName)
      val returnItem = items(itemLocation)
      items = items.filterNot(_.name == itemName)
      Some(returnItem)

    } else None
  }

  def dropItem(item: Item): Unit = {
    items = item :: items

  }

}

object Room {
  case class LinkExits(rooms: Map[String, ActorRef])
  case class GetExit(dir: Int)
  case class DropItem(item: Item)
  case class FindPlayer(p: String)
  case class AddPlayer(p: ActorRef)
  case class DropPerson(p: ActorRef, id:String)
  case class GetItem(itemName: String)
  case class Say(m: String, p: ActorRef)
  case class AddNPC(n: ActorRef)
  case class Action(a: String)
  case object InitNPC
  case object MoveNPC
  case object PrintDescription
}