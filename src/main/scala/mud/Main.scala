package mud

import scala.io.StdIn._
import akka.actor.ActorSystem
import akka.actor.Props
import scala.concurrent.duration._
import java.io.BufferedReader
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import java.net.ServerSocket
import scala.concurrent.Future
import java.io.PrintStream
import java.io.InputStreamReader

object Main extends App {

  def welcome(): String = {
    """                WELCOME TO
   _    _ _                  _                              _ _     
  | |  | (_)                | |                            | | |    
  | |  | |_ ______ _ _ __ __| |___   _ __    ___ _ __   ___| | |___ 
  | |/\| | |_  / _` | '__/ _` / __| | '_ \  / __| '_ \ / _ \ | / __|
  \  /\  / |/ / (_| | | | (_| \__ \ | | | | \__ \ |_) |  __/ | \__ \
   \/  \/|_/___\__,_|_|  \__,_|___/ |_| |_| |___/ .__/ \___|_|_|___/
                                                | |                 
                                                |_|                 """
  }
  val system = ActorSystem("MudSystem")
  val roomSuper = system.actorOf(Props[RoomSupervisor], "RoomSuper")
  val playerSuper = system.actorOf(Props[PlayerSupervisor], "PlayerSuper")
  val activitySuper = system.actorOf(Props[ActivitySupervisor], "ActivitySuper")
  implicit val timeout = Timeout(1.seconds)
  implicit val ec = system.dispatcher

  system.scheduler.schedule(0.seconds, 0.1.seconds, playerSuper, PlayerSupervisor.ProcessInput)
  system.scheduler.schedule(0.seconds, 0.1.seconds, activitySuper, ActivitySupervisor.CheckPQ)
  system.scheduler.schedule(1.seconds, 10.seconds, roomSuper, RoomSupervisor.MoveNPCs)
  val ss = new ServerSocket(4004)
  var names = new MyDLL[String]
  while (true) {
    val sock = ss.accept()
    
    Future {
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      out.println(welcome)
      
      var name: String = ""
      out.print("What is your name?\n>")
      while(name == ""){ 
        name = in.readLine().toLowerCase()
        if(names.filter(_ ==name).length != 0){
          out.print("That name is already taken\n>")
          name = ""
        }
        if(name.contains(" ")){
          out.print("Names must be one word\n>")
          name = ""
        }
      }
      names.+=(name)
 
      playerSuper ! PlayerSupervisor.NewPlayer(List[Item](), sock, in, out, name)
    }
  }
}
