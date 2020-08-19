package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import mud.Room.AddNPC

class RoomSupervisor extends Actor {
  import RoomSupervisor._
  var NPCs = List[ActorRef]()
  var pathMap = scala.collection.mutable.Map[String, Array[String]]()
  //roomKeys = scala.collection.mutable.Map[Room, String]()
  val rooms = {
    val xmlData = xml.XML.loadFile("map.xml")
    (xmlData \ "room").map(n => {
      val name = (n \ "@name").text
      val keyword = (n \ "@keyword").text
      val desc = (n \ "description").text
      val exits = (n \ "exits").text.split(",")
      val npcs = (n \ "npcs").text
      val items = (n \ "item").map(in =>
        Item((in \ "@name").text, in.text, (in \ "@weapon").text.toBoolean,
          (in \ "@dmg").text.toInt, (in \ "@speed").text.toInt)).toList
      keyword -> context.actorOf(Props(new Room(name, keyword, desc, exits, items, npcs)), keyword)
      //roomKeys(keyword) 
    }).toMap
  }
  val roomKeys = rooms.map(_.swap)

  context.children.foreach(p =>
    (p ! Room.LinkExits(rooms),
      p ! Room.InitNPC))

  def receive = {
    case StartPlayer(p) =>
      p ! Player.SetRoom(rooms("bedroom"))
      rooms("bedroom") ! Room.AddPlayer(p)
    case ShortestPath(r, k) =>
      if (pathMap.contains(k)) {
        sender ! Player.PrintMessage(shortestPath(r, k))
      }
    case newNPC(npcname, k, room) =>
      val n = context.actorOf(Props(new NPC(npcname, room)), npcname)
      NPCs = n :: NPCs
      rooms(k) ! AddNPC(n)
    case NewRoomData(k, rExits) =>
      val key = roomKeys(k)
      pathMap(k.path.name) = rExits map {
        case None => "-"
        case j => j.get.path.name
      }
    case MoveNPCs =>
      context.children.foreach(p => if (!NPCs.contains(p)) p ! Room.MoveNPC)
    case m =>
      println("RoomSupervisor got bad message: " + m)

  }
  def shortestPath(vertex1: String, vertex2: String): String = {
    var ret = List[String]()
    def helper(vert: String, visited: Set[String], dir: Int): Int = {
      var steps = 1000000
      if (vert == vertex2) {
        ret = checkDir(dir) :: ret
        0
      } else {
        val newVisited = visited + vert
        val exits = pathMap(vert).toList
        for (e <- 0 to 5) {
          println(ret)
          if (exits(e) != "-" && !newVisited(exits(e))) {
            steps = steps min helper(exits(e), newVisited, e)
            
          }
        }
        if (steps < 1000000) {
              println(vertex1, vertex2, checkDir(dir))
              ret = checkDir(dir) :: ret
            }
        println("steps: " + steps)
        steps + 1

      }

    }

    if (vertex1 == vertex2) {
      "You're already in that room!"
    }
    if (pathMap.contains(vertex2)) {
      helper(vertex1, Set(), pathMap(vertex1).indexWhere(p => p != "-"))
      ret.mkString(", ")
    } else "That room doesn't exist"

  }
  def checkDir(dir: Int): String = {
    dir match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
      case 4 => "Up"
      case 5 => "Down"
    }
  }
  def connects(vertex1: String, vertex2: String): Boolean = {
    val exits = pathMap(vertex1)
    exits.foreach(e => if (e == vertex2) true)
    false
  }
}

object RoomSupervisor {
  case object MoveNPCs
  case class StartPlayer(p: ActorRef)
  case class ShortestPath(r: String, k: String)
  case class NewRoomData(key: ActorRef, rExits: Array[Option[ActorRef]])
  case class newNPC(name: String, k: String, room: ActorRef)
}