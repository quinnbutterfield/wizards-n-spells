package mud

import scala.io.StdIn._


import akka.actor.ActorRef
import akka.actor.Actor
import akka.pattern.ask
import java.io.BufferedReader
import java.io.PrintStream
import java.net.Socket
import akka.actor.PoisonPill
import mud.Room.GetExit

class Player(
    private var inventory: List[Item],
    sock: Socket,
    in: BufferedReader,
    out: PrintStream,
    name: String) extends Actor {
  private val id = "player"
  private var health: Int = 50
  private var location: ActorRef = null
  private var currentWep: Item = null
  private var target: ActorRef = null
  private var inCombat: ActorRef = null
  private val hands: Item = new Item("bare hands", "the weapons you were born with", true, 1, 5)
  val rand = new scala.util.Random
  import Player._
  def receive = {
    case ProcessInput =>
      if (health <= 0) {
        inCombat = null
        out.println("You died!")
        Main.activitySuper ! ActivitySupervisor.Dead(self)
        location ! Room.DropPerson(self, id)
        location ! Room.Action("died.")
        self ! PoisonPill
        sock.close()
      }
      if (in.ready()) {

        val input = in.readLine.toLowerCase()
        processCommand(input)
      }
    case SetRoom(room: ActorRef) =>
      location = room
      showRoom()
    case NotInCombat =>
      this.inCombat = null
    case InRoom(p) =>
      target = p
      inCombat = target
      if (currentWep == null) currentWep = hands
      Main.activitySuper ! ActivitySupervisor.ScheduleActivity(currentWep.speed * 2, self, Kill)
    case Kill =>
      if(target == self){
        self ! PrintMessage("That's probably a bad idea.")
        inCombat = null
      }
      else if(inCombat == target){
        val dmg = currentWep.damage * rand.nextInt(10)
        target ! Attack(currentWep, dmg)
        self ! PrintMessage("You attacked " + target.path.name + " with your " + currentWep.name)
      }
    case Attack(weapon, dmg) =>
      if (inCombat == null || inCombat == sender) {
        out.println("You are being attacked by " + sender.path.name + "'s " + weapon.name)
        out.println("It did " + dmg + " damage!")
        sender ! PrintMessage("It did " + dmg + " damage!")
        lowerHealth(dmg)
        out.println("Your health is now " + health)
        inCombat = sender
        target = sender
        if (currentWep == null) currentWep = hands
        location ! Room.FindPlayer(sender.path.name)
        //Main.activitySuper ! ActivitySupervisor.ScheduleActivity(currentWep.speed*10, self, Kill)
      }
    case PrintMessage(message: String) =>
      out.println(message)
      out.print(">")
    case TakeExit(dest) =>
      dest match {
        case Some(dest) =>
          inCombat = null
          if(target != null) target ! NotInCombat
          location ! Room.DropPerson(self, id)
          location ! Room.Action("left the room.")
          location = dest
          dest ! Room.AddPlayer(self)
          dest ! Room.Action("entered ")
          showRoom()
          out.println(">")
        case None =>
          out.println("You can't go that way")
          out.print(">")
      }
    case GotItem(possibleItem) =>
      possibleItem match {
        case Some(item) =>
          addToInventory(item)
          location ! Room.Action("picked up the " + item.name)
        case None => out.print("That item isn't here.\n>")

      }
    case m => println(s"Bad message in Player: $m")
  }
  def processCommand(command: String): Unit = {
    command match {
      case "north" | "n" => move("north")
      case "east" | "e" => move("east")
      case "south" | "s" => move("south")
      case "west" | "w" => move("west")
      case "up" => move("up")
      case "down" => move("down")
      case "look" | "l" =>
        showRoom()
      case "inv" | "inventory" =>
        out.println(inventoryListing())
        out.print(">")
      case "help" => help()
      case s if s.startsWith("get ") =>
        location ! Room.GetItem(s.stripPrefix("get "))
      case s if s.startsWith("drop ") => getFromInventory(s.stripPrefix("drop ")) match {
        case Some(item) => {
          location ! Room.DropItem(item)
          location ! Room.Action("dropped the " + item.name)
        }
        case None => out.print("You don't have that item\n>")
      }
      case s if s.startsWith("say ") =>
        if (s.stripPrefix("say ").length() != 0) {
          location ! Room.Say(s.stripPrefix("say "), self)
        }
        out.print(">")
      case s if s.startsWith("tell ") =>
        try {
          var msg = s.split(" ")
          msg = msg.tail
          Main.playerSuper ! PlayerSupervisor.Tell(msg(0), msg.tail.mkString(" "))
          out.print(">")
        } catch {
          case _: Throwable => out.print("format: tell user message\n>")
        }
      case "exit" =>
        location ! Room.DropPerson(self, id)
        location ! Room.Action("logged out")
        self ! PoisonPill
        sock.close()
      case a if a.startsWith("kill ") =>
        if (a.stripPrefix("kill ").length() != 0) {
          location ! Room.FindPlayer(a.split(" ")(1))
        }
      case "flee" =>
        if (inCombat == null) out.print("You don't have anything to flee from!\n>")
        else {
          location ! GetExit(rand.nextInt(6))
        }
      case a if a.startsWith("shortestpath") =>
        if (a.stripPrefix("shortestpath").length() !=0){
          Main.roomSuper ! RoomSupervisor.ShortestPath(location.path.name, a.split(" ")(1))
        }
      case "getname" => getName()
      case "health" | "hp" => out.print("Your health is " + health +"\n>")
      case e if e.startsWith("equip ") => getFromInventory(e.stripPrefix("equip ")) match {
        case Some(item) =>
          equip(item)
        case None => out.print("You don't have that weapon\n>")
      }
      case e if e.startsWith("unequip ") =>
        if (currentWep == null) out.print("You don't have a weapon equipped\n>")
        else if (currentWep.name == e.stripPrefix("unequip ")) {
          unequip(currentWep)

        }

      case _ => out.print(">")
    }
  }
  def help(): Unit = {
    out.println("""
            north or n ------- go north
            east or e -------- go east
            south or s ------- go south
            west or w -------- go west
            up --------------- go up
            down ------------- go down
            get (item) ------- pick up an item if it exists
            drop (item) ------ drop an item if it is in your inventory
            equip (item) ----- equip an item if it is in your inventory
            unequip (item) --- unequip an item if it is in your inventory
            kill (player) ---- attack a player or NPC if they are in the room
            look ------------- see your surroundings
            inventory or inv - check your inventory
            exit ------------- exit the game
            getname ---------- check your name
            health ----------- check your current health
            """)
    out.print(">")
  }
  def lowerHealth(i: Int): Unit = health -= i
  def raiseHealth(i: Int): Unit = health += i
  def getFromInventory(itemName: String): Option[Item] = {
    val invNames = inventory.map(_.name)
    if (invNames.contains(itemName)) {
      val returnItem = inventory(invNames.indexOf(itemName))
      inventory = inventory.filterNot(_.name == itemName)
      Some(returnItem)
    } else None
  }
  def addToInventory(item: Item): Unit = {
    inventory = item :: inventory
  }
  def equip(item: Item): Unit = {
    if (item.isWeapon) {
      if (currentWep != null) unequip(currentWep)
      currentWep = item
      inventory = inventory.filterNot(_ == item)
      out.print("You equipped the " + item.name + "\n>")
    } else out.print("You can't equip that!\n>")

  }
  def unequip(item: Item): Unit = {
    inventory = currentWep :: inventory
    out.print("You unequipped the " + currentWep.name + "\n>")
    currentWep = null
  }
  def showRoom(): Unit = {
    location ! Room.PrintDescription
  }
  def getName(): Unit = {
    out.print("Your name is " + name + "\n>")
  }

  def inventoryListing(): String = {
    out.println("Inventory:")
    if (inventory.length == 0) "Your pockets are empty"
    else inventory.map(i => i.name + " - " + i.description).mkString("\n")
  }
  def move(dir: String): Unit = {
    if (inCombat != null) {
      out.print("You can't move while in combat!\n>")
    }
    else location ! Room.GetExit(dir match {
      case "north" => 0
      case "south" => 1
      case "east" => 2
      case "west" => 3
      case "up" => 4
      case "down" => 5
    })
  }
}
object Player {
  case object Kill
  case object TargetDead
  case object NotInCombat
  case class Attack(weapon: Item, dmg: Int)
  case class SetRoom(room: ActorRef)
  case class InRoom(p: ActorRef)
  case class TakeExit(dest: Option[ActorRef])
  case object ProcessInput
  case class PrintMessage(message: String)
  case class GotItem(item: Option[Item])
}
