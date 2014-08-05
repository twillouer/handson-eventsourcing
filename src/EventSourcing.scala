

object Deck {

  object Color extends Enumeration {
    type Color = Value
    val Red, Green, Yellow, Blue = Value
  }

  object Direction extends Enumeration {
    type Direction = Value
    val ClockWise, CounterClockWise = Value
  }

  type Digit = Int

  sealed abstract class Card(color: Color.Color)
  case class CardDigit(color: Color.Color, digit: Digit) extends Card(color)
  case class CardKickBack(color: Color.Color) extends Card(color)

  type GameId = Int
  type PlayerId = Int

  sealed abstract class Command(gameId:GameId)
  case class StartGame(gameId: GameId, playerCount: Int, firstCard: Card) extends Command(gameId)
  case class PlayCard(gameId: GameId, playerId: PlayerId, card: Card) extends Command(gameId)


  sealed abstract class Event(gameId:GameId)
  case class GameStarted(gameId: GameId, playerCount: Int, firstCard: Card) extends Event(gameId)
  case class CardPlayed(gameId: GameId, playerId: PlayerId, card: Card) extends Event(gameId)
}

/**
  */
object EventSourcing extends App {

  import Deck.Color._
  import Deck._

  val colors = CardDigit(Red, 1) :: CardDigit(Blue, 5) :: Nil

  println("Coucou")
}
