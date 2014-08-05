import scala.util.{Success, Try}

object Deck {

  object Color extends Enumeration {
    type Color = Value
    val Red, Green, Yellow, Blue = Value
  }

  sealed trait Direction {
    def nextPlayer(player: PlayerId, playerCount: Int): Int

    def nextDirection: Direction
  }

  object ClockWise extends Direction {
    def nextPlayer(player: PlayerId, playerCount: Int): Int = (player + 1) % playerCount

    def nextDirection = CounterClockWise
  }

  object CounterClockWise extends Direction {
    def nextPlayer(player: PlayerId, playerCount: Int): Int = (player - 1) % playerCount

    def nextDirection = ClockWise
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

    def direction: Direction
  }

  object EmptyState extends State {
    val isStarted = false

    def nextPlayer = throw new IllegalAccessException()

    def lastCard = throw new IllegalAccessException()

    def direction: Direction = throw new IllegalAccessException()
  }

  case class PlayedState(playerCount: Int, nextPlayer: PlayerId, lastCard: Card, direction: Direction) extends State {
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
    else
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
    (event, state) match {
      case (gs: GameStarted, _) => new PlayedState(gs.playerCount, 1, gs.firstCard, ClockWise)
      case (gs: CardPlayed, ps: PlayedState) =>
        val direction =
          gs.card match {
            case kick: CardKickBack => state.direction.nextDirection
            case _ => state.direction
          }
        ps.copy(nextPlayer = direction.nextPlayer(ps.nextPlayer, ps.playerCount), lastCard = gs.card, direction = direction)
      case (pf: PlayerFailed, _) => state
      case _ => ???
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
    val simpleDeckKickBack: List[Event] = GameStarted(1, 4, CardDigit(3, Red)) :: CardPlayed(1, 1, CardDigit(9, Red)) :: CardPlayed(1, 2, CardKickBack(Red)) :: Nil
    val simpleDeckKickBackAndMore: List[Event] = GameStarted(1, 4, CardDigit(3, Red)) :: CardPlayed(1, 1, CardDigit(9, Red)) :: CardPlayed(1, 2, CardKickBack(Red)) :: CardPlayed(1, 1, CardDigit(9, Red)) :: Nil
  }


  def when(given: List[Event], command: Command): Event = {
    decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), command)
  }

  case class Given(given: List[Event])

  def test(given: List[Event])(when: Command)(then: Try[Event] => Boolean) = {
    val result = Try(decide(given.foldLeft(EmptyState.asInstanceOf[State])((s, event) => appli(s, event)), when))
    println(result)
    require(then(result))
  }

  def started_game = test(Given.emptyDeck)(StartGame(1, 4, CardDigit(3, Red)))(_ == Success(GameStarted(1, 4, CardDigit(3, Red))))

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

  def next_card_is_same_color = test(Given.simpleDeck)(PlayCard(1, 2, CardDigit(3, Red)))(_ == Success(CardPlayed(1, 2, CardDigit(3, Red))))

  def next_card_is_same_number = test(Given.simpleDeck)(PlayCard(1, 2, CardDigit(9, Yellow)))(_ == Success(CardPlayed(1, 2, CardDigit(9, Yellow))))

  def next_card_is_not_same_color_not_number_but_same_player = test(Given.simpleDeck)(PlayCard(1, 2, CardDigit(3, Yellow)))(_ == Success(PlayerFailed(1, 2, CardDigit(3, Yellow))))

  def next_card_is_not_same_color_not_number = test(Given.simpleDeck)(PlayCard(1, 3, CardDigit(3, Yellow)))(_ == Success(PlayerFailed(1, 3, CardDigit(3, Yellow))))

  def bad_player_try_to_play = test(Given.simpleDeck)(PlayCard(1, 1, CardDigit(3, Yellow)))(_ == Success(PlayerFailed(1, 1, CardDigit(3, Yellow))))

  def kickback_must_change_direction_next_player_failed = test(Given.simpleDeckKickBack)(PlayCard(1, 2, CardDigit(3, Red)))(_ == Success(PlayerFailed(1, 2, CardDigit(3, Red))))

  def kickback_must_change_direction_next_player_ok = test(Given.simpleDeckKickBack)(PlayCard(1, 1, CardDigit(3, Red)))(_ == Success(CardPlayed(1, 1, CardDigit(3, Red))))

  def kickback_can_be_added_after_a_kickback = test(Given.simpleDeckKickBack)(PlayCard(1, 1, CardKickBack(Yellow)))(_ == Success(CardPlayed(1, 1, CardKickBack(Yellow))))

  def kickback_is_durability_saved = test(Given.simpleDeckKickBackAndMore)(PlayCard(1, 0, CardDigit(9, Yellow)))(_ == Success(CardPlayed(1, 0, CardDigit(9, Yellow))))

  cannot_start_both
  enough_players
  not_enough_players
  started_game
  next_card_is_same_color
  next_card_is_same_number
  next_card_is_not_same_color_not_number
  next_card_is_not_same_color_not_number_but_same_player
  bad_player_try_to_play
  kickback_must_change_direction_next_player_failed
  kickback_must_change_direction_next_player_ok
  kickback_can_be_added_after_a_kickback
  kickback_is_durability_saved
}