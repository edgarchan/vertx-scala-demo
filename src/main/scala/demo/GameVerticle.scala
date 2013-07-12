package demo

import org.vertx.scala.core.eventbus.Message
import org.vertx.scala.platform.Verticle
import org.vertx.java.core.AsyncResult
import org.vertx.java.core.json.JsonObject
import scala.collection.concurrent.TrieMap

abstract class Move(val name:String)
case object Rock extends Move("Rock")
case object Paper extends Move("Paper")
case object Scissors extends Move("Scissors")
case object Noop extends Move("Noop")
case class Round(p1Move:Move, p2Move:Move)


class GameVerticle extends Verticle {
 
  private val rounds  = new TrieMap[String, Round]()  
  
  def toMove(in:String):Move = in match{
      case Rock.name     =>  Rock
      case Paper.name    =>  Paper
      case Scissors.name =>  Scissors
      case _ => Noop
  }
  
  def announce(r:Round, winner:String, num:String){
    val p1 = r.p1Move.name
    val p2 = r.p2Move.name
    println(s"Round ..$num")
    println(s"Player1: $p1, Player2: $p2 ... the winner is $winner")
  }
  
  override def start() {

    container.logger().info("Starting game...")
    
    vertx.eventBus.registerHandler("game")(    
     (msg: Message[JsonObject]) => {
       
       val th = msg.body.getString("throw")
       val py = msg.body.getString("player")
       val rd = msg.body.getString("round")
       
       
       Option(py).collect{
         case px1 if px1 == "Player1" && rounds.contains(rd) =>
            rounds.replace(rd, rounds(rd).copy(p1Move = toMove(th)))
         case px2 if px2 == "Player1"  =>            
            rounds.update(rd, Round(toMove(th), Noop))
         case py1 if py1 == "Player2" && rounds.contains(rd) =>
            rounds.replace(rd, rounds(rd).copy(p2Move = toMove(th)))
         case py2 if py2 == "Player2"  =>            
            rounds.update(rd, Round(Noop, toMove(th)))                    
       }                      
       
       rounds(rd) match{
            case rp @ Round(Rock, Paper)     =>   announce(rp, "Player2", rd)
            case rs @ Round(Rock, Scissors)  =>   announce(rs, "Player1", rd)
            case pr @ Round(Paper,Rock)      =>   announce(pr, "Player1", rd)
            case ps @ Round(Paper,Scissors)  =>   announce(ps, "Player2", rd)
            case sr @ Round(Scissors, Rock)  =>   announce(sr, "Player2", rd)
            case sp @ Round(Scissors, Paper) =>   announce(sp, "Player1", rd)   
            case tie @ Round(_, _) if  tie.p1Move == tie.p2Move  => announce(tie, "tie", rd)
            case _ => {}
       }
        
    })
    
    
    container.deployVerticle(
      "scala:demo.Player1"
    )
    
    container.deployVerticle(
      "scala:demo.Player2"
    )
    
    vertx.timer(200)(t => {       
      (1 to 10).foreach { i =>
        vertx.eventBus.publish("start", s"$i")   
      }
    })
    
  }
  
}



trait Player extends Verticle{
  val name:String
  val random = scala.util.Random
  val throws = List(Rock, Scissors, Paper)  
  
  def action:PartialFunction[Message[String], Unit] = {
    case a => {
      val r = a.body
      val t = throws(random.nextInt(throws.length)).name
      val json = new JsonObject(s"""{"player":"$name", "throw":"$t", "round":"$r"}""")
      vertx.eventBus.publish("game", json)      
    } 
  }
  
  override def start(){
    vertx.eventBus.registerHandler("start")(action)
  }
    
}


class Player1 extends Player { val name = "Player1" }
class Player2 extends Player { val name = "Player2" }
