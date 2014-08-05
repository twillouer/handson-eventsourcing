import Deck.Color._
import Deck._

/**
  */
class EventSourcingSpec extends App {


  object Given {
    val simpleDeck = GameStarted(1, 4, CardDigit(3, Red)) :: CardPlayed(1, 0, CardDigit(9, Red)) :: Nil

  }


  println("coucou")

}
