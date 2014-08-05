import scala.util.Try

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

  sealed trait Card {
    def color: Color.Color
  }

  case class CardDigit(digit: Digit, color: Color.Color) extends Card {
  }

  case class CardKickBack(color: Color.Color) extends Card

  type GameId = Int
  type PlayerId = Int

  sealed trait Command {
    def gameId: GameId
  }

  case class StartGame(gameId: GameId, playerCount: Int, firstCard: Card) extends Command

  case class PlayCard(gameId: GameId, playerId: PlayerId, card: Card) extends Command


  sealed abstract class Event(gameId: GameId)

  case class GameStarted(gameId: GameId, playerCount: Int, firstCard: Card) extends Event(gameId)

  case class CardPlayed(gameId: GameId, playerId: PlayerId, card: Card) extends Event(gameId)

  case class PlayerFailed(gameId: GameId, playerId: PlayerId, card: Card) extends Event(gameId)

}

/**
  */
object EventSourcing extends App {

  import Deck.Color._
  import Deck._

  val colors = CardDigit(1, Red) :: CardDigit(5, Blue) :: Nil


  sealed trait State {
    def isStarted: Boolean

    def nextPlayer: PlayerId

    def lastCard: Card
  }

  object EmptyState extends State {
    val isStarted = false

    def nextPlayer = throw new IllegalAccessException()

    def lastCard = throw new IllegalAccessException()
  }

  case class PlayedState(playerCount: Int, nextPlayer: PlayerId, lastCard: Card) extends State {
    val isStarted = true
  }

  def decide(state: State, command: Command): Event = {
    command match {
      case sg: StartGame if state.isStarted => throw new Exception("Not Started")
      case sg: StartGame if sg.playerCount < 3 => throw new Exception("Need more player")
      case sg: StartGame =>
        GameStarted(sg.gameId, sg.playerCount, sg.firstCard)
      case pc: PlayCard if pc.playerId != state.nextPlayer =>
        PlayerFailed(pc.gameId, pc.playerId, pc.card)
      case pc: PlayCard if !cardLegal(state.lastCard, pc.card) =>
        PlayerFailed(pc.gameId, pc.playerId, pc.card)
      case pc: PlayCard =>
        CardPlayed(pc.gameId, pc.playerId, pc.card)
    }
  }

  /**
   * True if card is legal with topCard.
   */
  def cardLegal(lastCard: Card, newCard: Card): Boolean = {
    if (lastCard.color == newCard.color) true

    lastCard match {
      case ckb: CardKickBack => newCard.isInstanceOf[CardKickBack]
      case cd: CardDigit =>
        newCard match {
          case newCardCD: CardDigit => newCardCD.digit == cd.digit
          case _ => false
        }
    }
  }

  def appli(state: State, event: Event): State = {
    event match {
      case gs: GameStarted => new PlayedState(gs.playerCount, 0, gs.firstCard)
      case gs: CardPlayed =>
        state match {
          case ps: PlayedState => ps.copy(nextPlayer = (ps.nextPlayer + 1) % ps.playerCount, lastCard = gs.card)
          case _ => ???
        }
      case pf: PlayerFailed => state
    }
  }

  println("Coucou")
}


object EventSourcingTest extends App {

  import Deck.Color._
  import Deck._
  import EventSourcing._

  object Given {
    val emptyDeck: List[Event] = Nil
    val startedDeck: List[Event] = GameStarted(1, 4, CardDigit(3, Red)) :: Nil
    val simpleDeck: List[Event] = GameStarted(1, 4, CardDigit(3, Red)) :: CardPlayed(1, 0, CardDigit(9, Red)) :: Nil
  }


  def started_game {
    // Given
    val given = Given.emptyDeck
    val command = StartGame(1, 4, CardDigit(3, Red))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == GameStarted(1, 4, CardDigit(3, Red)))
  }

  def cannot_start_both {
    // Given
    val given = Given.startedDeck
    val command = StartGame(1, 4, CardDigit(3, Red))

    // When
    val result = Try(decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command))

    // Then
    require(result.isFailure)
  }

  def not_enough_players {
    // Given
    val given = Given.emptyDeck
    val command = StartGame(1, 1, CardDigit(3, Red))


    // When
    val result = Try(decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command))

    // Then
    require(result.isFailure)
  }

  def enough_players {
    // Given
    val given = Given.emptyDeck
    val command = StartGame(1, 3, CardDigit(3, Red))


    // When
    val result = Try(decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command))

    // Then
    require(result.isSuccess)
  }

  def next_card_is_same_color {
    // Given
    val given = Given.simpleDeck
    val command = PlayCard(1, 1, CardDigit(3, Red))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == CardPlayed(1, 1, CardDigit(3, Red)))
  }

  def next_card_is_same_number {
    // Given
    val given = Given.simpleDeck
    val command = PlayCard(1, 1, CardDigit(9, Yellow))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == CardPlayed(1, 1, command.card))
  }

  def next_card_is_not_same_color_not_number_but_same_player {
    // Given
    val given = Given.simpleDeck
    val command = PlayCard(1, 1, CardDigit(3, Yellow))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == PlayerFailed(1, 1, command.card))
  }

  def next_card_is_not_same_color_not_number {
    // Given
    val given = Given.simpleDeck
    val command = PlayCard(1, 3, CardDigit(3, Yellow))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == PlayerFailed(1, 3, command.card))
  }

  def bad_player_try_to_play {
    // Given
    val given = Given.simpleDeck
    val command = PlayCard(1, 1, CardDigit(3, Yellow))

    // When
    val result = decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)

    // Then
    println(result)
    require(result == PlayerFailed(1, 1, command.card))
  }

  def started_game_should_be_started {
    // Given
    val given = Given.simpleDeck

    // When
    val result = given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event))

    // Then
    println(result)
    result == ???
  }


  cannot_start_both
  enough_players
  not_enough_players
  started_game
  next_card_is_same_color
  next_card_is_same_number
  next_card_is_not_same_color_not_number
  next_card_is_not_same_color_not_number_but_same_player
  bad_player_try_to_play
  started_game_should_be_started
}