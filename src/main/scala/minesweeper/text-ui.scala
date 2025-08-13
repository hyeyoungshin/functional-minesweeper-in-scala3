package minesweeper

import scala.io.StdIn.readLine
import java.awt.Color
import upickle.implicits.Readers
import minesweeper.game.*

///////////////////////////////////////////////////////////////
//////////// Text based Single Player Minesweeper /////////////
///////////////////////////////////////////////////////////////
@main def text_ui_game(): Unit = 
  print_start()
  
  val difficulty = get_valid_difficulty()
  print_difficulty(difficulty)

  var state = new_game(difficulty)
  // print_state(state)

  while !game_over(state.status) do 
    // before making a move print a board
    print_playerstate(state.playerpool.current_playerstate(), state.solution.num_mines)
    // get move
    val player_action = get_valid_inputs(state)
    // play
    val new_state = play(state, player_action)
    // after move print a board
    print_playerstate(new_state.playerpool.current_playerstate(), new_state.solution.num_mines)
    // evaluate 
    // val evaluated = evaluate_state(new_state) // TODO: this should be outside the game loop
    // report
    // print_state(evaluated)
    // ready for next player
    state = next_player(new_state)

  // here, state.status is Win(winners) by 1. revealed a mine 2. all mines are flagged
  val result = update_with_result(state)

  print_state(result)


//////////////////////////////////
////// Game Start and Setup //////
//////////////////////////////////  
def print_start(): Unit = {
  println("")
  print_with_effect("Welcome to the minesweeper game.", PrinterEffects.Bold)
}


//////////////////////////////
// Parse and Validate Input //
//////////////////////////////
/* Used to parse and validate user input for T which is one of
 - GameDifficulty   
 - Coordinate
 - PlayerAction
*/
def get_valid_input[T](message: String, parse_and_validate: String => Either[String, T]): T = {
  def loop(): T = 
    print_with_effect(message, PrinterEffects.Bold)
    val player_input = readLine()
    parse_and_validate(player_input) match {
      case Right(value) => value
      case Left(msg) => print_with_effect(msg, PrinterEffects.Red); loop()
    }
  
  loop()
}


def get_valid_inputs(state: GameState): PlayerAction = {
  def loop(): PlayerAction = 
    val coordinate = get_valid_coordinate(state)
    val action = get_valid_action()
    valid_player_action(state, PlayerAction(action, coordinate)) match {
      case Right(action) => action
      case Left(msg) => print_with_effect(msg, PrinterEffects.Bold); loop()
    }

  loop()
}


/////////////////////
// Game Difficulty //
/////////////////////  
def get_valid_difficulty(): GameDifficulty = {
  get_valid_input("Choose difficulty: (Easy, Intermediate, Hard)", parse_difficulty)
}


def parse_difficulty(user_input: String): Either[String, GameDifficulty] = {
  user_input match {
    case "Easy" => Right(Easy)
    case "Intermediate" => Right(Medium)
    case "Hard" => Right(Hard)
    case _ => Left("Enter Easy, Intermediate, or Hard only.")
  }
}


////////////////
// Coordinate //
////////////////
def get_valid_coordinate(state: GameState): Coordinate = {
  val current_player = state.playerpool.current_player()

  get_valid_input(message = s"Player ${current_player.id}, enter a tile position:",
                  parse_and_validate = input => parse_coordinate(input).flatMap(x => valid_coordinate(state, x)))
}
    

/* parse user input via int array
for example, 1,1 is [1,1]
int array of length other than 2 is considered invalid
*/ 
def parse_coordinate(user_input: String): Either[String, Coordinate] =
  val option_parsed = parse_coordinate_helper(user_input)
  option_parsed.filter(arr => arr.length == 2) match {
    case Some(arr) => Right(convert_input_coordinate_to_coordinate(InputCoordinate(arr(0), arr(1))))
    case None => Left("The coordinate you entered is in wrong format.")
  }


/* handle cases where user type non numeric coordinates */
def parse_coordinate_helper(user_input: String): Option[Array[Int]] = 
  try {
    Some(user_input.split(",").map(_.toInt))
  } catch {
    case _: NumberFormatException => None
  }


/* check user input coordinate is valid
user input coordinate is valid when it is within board boundary AND
the tile at input coordinate is not revealed
 */
def valid_coordinate(state: GameState, tile_pos: Coordinate): Either[String, Coordinate] = 
  val current_playerboard = state.playerpool.current_playerboard()
  if current_playerboard.withinBoundary(tile_pos) then
    current_playerboard.tileMap(tile_pos) match {
      case PlayerTile.Revealed(_) => Left("The tile is alreay revealed.")
      case PlayerTile.RevealedNFlagged(_, _) => Left("The tile is alreay revealed.")
      case _ => Right(tile_pos)
    }
  else 
    Left("The tile position is outside the boundary.")



///////////////////
// Player Action //
///////////////////
def get_valid_action(): Action = 
  get_valid_input(message = "Enter an action: R for reveal, F for flag, U for unflag",
                  parse_and_validate = input => parse_action(input))
                  

def parse_action(input: String): Either[String, Action] = 
  input match {
    case "R" => Right(Action.Reveal)
    case "F" => Right(Action.Flag)
    case "U" => Right(Action.Unflag)
    case _ => Left("Please only enter R, F, or U: ")
  }


def valid_player_action(state: GameState, player_action: PlayerAction): Either[String, PlayerAction] = {
  val current_player = state.playerpool.current_player()
  val current_playerboard = state.playerpool.current_playerboard()
  val p_tile = current_playerboard.tileMap(player_action.tile_pos)
  

  player_action.action match {
    case Action.Flag => p_tile match {
      case PlayerTile.Hidden => Right(player_action) 
      case PlayerTile.Flagged(flagger) => Left(s"The tile is already flagged by Player ${flagger.id}.")
      case PlayerTile.Revealed(_) => Left(s"The tile is already revealed.")
      case PlayerTile.RevealedNFlagged(_, _) => Left(s"The tile is already revealed.")

      
      // case PlayerTile.Hidden => Right(player_action) 
      // case PlayerTile.Flagged(by) => Left(s"The tile is already flagged by Player ${by.id}.")
      // case PlayerTile.Revealed(tile) => Left(s"The tile is already revealed.")
    }
    
    case Action.Reveal => p_tile match {
      case PlayerTile.Hidden => Right(player_action) 
      case PlayerTile.Flagged(flagger) =>
        if current_player != flagger then 
          Right(player_action) // you can reveal a tile flagged by another player
        else
          Left(s"The tile is already flagged.")
      case PlayerTile.Revealed(_) => Left(s"The tile is already revealed.")
      case PlayerTile.RevealedNFlagged(_, _) => Left(s"The tile is already revealed.")
    }
    
    case Action.Unflag => p_tile match {
      case PlayerTile.Flagged(flagger) => 
        if current_player == flagger then
          Right(player_action)
        else
          Left(s"The tile is already flagged by Player ${flagger.id}.") 
      case PlayerTile.RevealedNFlagged(_, _) => Left("The tile is already revealed.")
      case _ => Left("You cannot unflag a tile that's not flagged.") 
      }
    }
}



//////////////
// Printers //
//////////////
// object AnsiColor:
//   val Red = "\u001b[31m"
//   val Green = "\u001b[32m"
//   val Yellow = "\u001b[33m"
//   val Blue = "\u001b[34m"
//   val Magenta = "\u001b[35m"
//   val Cyan = "\u001b[36m"


object PrinterEffects:
  val Red = "\u001b[31m"
  val Blue = "\u001B[34m"
  val Bold = "\u001b[1m"
  val Reset = "\u001b[0m"


def print_with_effect(text: String, effect: String): Unit = {
  println(s"$effect$text${PrinterEffects.Reset}")
}


def print_inplace(): Unit = 
  // Clears the screen
  print("\u001b[2J")
  // Moves the cursor to the top-left corner
  print("\u001b[H")


def print_difficulty(difficulty: GameDifficulty): Unit = {
  println("")
  print_with_effect(s"Starting a game with board size: (${difficulty.board_size._1} x ${difficulty.board_size._2}) " +
    s"and Number of mines: ${difficulty.num_mines}", PrinterEffects.Bold)
  println("")
 
  Thread.sleep(1500)
}


def print_state(state: GameState): Unit = {
  state.playerpool.playerstates.map(playerstate => {
    print_with_effect(s"Board: Player ${playerstate.player.id}", PrinterEffects.Bold)
    // print_with_effect(s"Number of Mines: ${state.solution.num_mines}", PrinterEffects.Bold)

    print_board(playerstate.board)
  })//print_playerstate(playerstate, state.solution.num_mines))
  print_status(state.status)
}


// Prints a board for testing purposes
// i.e., for mines located at (Coordinate(0, 1), Coordinate(1, 2)), it prints out
// [ ][ ][]
// [*][ ][]
// [ ][*][]
def print_playerstate(playerstate: PlayerState, num_mines: Int): Unit = {
  // prints boilder plate
  print_inplace()

  print_with_effect(s"Board: Player ${playerstate.player.id}", PrinterEffects.Bold)
  print_with_effect(s"Number of Mines: ${num_mines}", PrinterEffects.Bold)

  print_board(playerstate.board)

  Thread.sleep(1000)
}


def print_status(status: GameStatus): Unit = 
  status match {
    case GameStatus.Win(winners) => winners.foreach(winner => print_with_effect(s"Player ${winner.id} win!", PrinterEffects.Bold))
    case GameStatus.Continue => ()
  }

def print_board(board: PlayerBoard): Unit = {
  val str_board = Array.fill(board.xsize)(Array.fill(board.ysize)(""))
  board.tileMap.map((tile_pos, tile) => str_board(tile_pos.y)(tile_pos.x) = tile_to_string(tile))
  print_as_matrix[String](str_board)
}


def tile_to_string[T](tile: T): String = tile match {
  case t: SolutionTile => t match {
    case SolutionTile.Empty => "0"
    case SolutionTile.Mine => "x"
    case SolutionTile.Hint(n) => s"$n"
  }
  case t: PlayerTile => t  match {
    case PlayerTile.Hidden => "[   ]"
    case PlayerTile.Flagged(flagger) => s"[ ${flagger.get_color()}F${PrinterEffects.Reset} ]"
    case PlayerTile.Revealed(s_tile) => s"[ ${tile_to_string(s_tile)} ]"
    case PlayerTile.RevealedNFlagged(s_tile, flagger) => s"[${tile_to_string(s_tile)}|${flagger.get_color()}F${PrinterEffects.Reset}]"
  }
  case t: Boolean => t match {
    case true => "[*]" // mine
    case false => "[ ]"
  }
}


// * Prints boards in the matrix form (Array of Arrays)
// ** You give
// board : Array of Arrays of type T, where T can be Int for input board or String for Tile
// ** You get
// printed board in matrix format 
def print_as_matrix[T](board: Array[Array[T]]): Unit = 
  println(board.map(_.mkString("")).mkString("\n"))
  println("")



// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)
