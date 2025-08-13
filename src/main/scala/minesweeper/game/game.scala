package minesweeper.game

import upickle.default.*

////////////////////////////
///// Data types: Game /////
////////////////////////////

case class GameState (val solution: Solution,         
                      val playerpool: PlayerPool,
                      val status: GameStatus)           

case class Solution(val board: SolutionBoard, val num_mines: Int)

enum GameStatus:
  case Win(winners: Set[Player]) // Note: List vs. Set  
  case Continue

case class GameDifficulty(val board_size: (Int, Int), val num_mines: Int)

final val Easy = GameDifficulty(board_size = (3, 3), num_mines = 2)
final val Medium = GameDifficulty(board_size = (5, 5), num_mines = 4)
final val Hard = GameDifficulty(board_size = (7, 7), num_mines = 9)


//////////////////////////////
///// Data types: Player /////
//////////////////////////////

// keeps track of the current player with the index `next`
// updates the current player's or everyone's board after each move 
//
// * Input
// players: the list of players in the game
// next: the index of the current player 
case class PlayerPool(playerstates: List[PlayerState], val current: Int) {
  def current_playerstate(): PlayerState = playerstates(current)

  def current_player(): Player = playerstates(current).player
  def current_playerboard(): PlayerBoard = playerstates(current).board
  
  // Updates the player pool by incrementing the index 
  def next_pool(): PlayerPool = this.copy(current = (current + 1) % playerstates.length)

  def get_players(): List[Player] = playerstates.map(playerstate => playerstate.player)

  // Returns the players except the current player
  // Useful for getting the winners if the current player lost
  def get_rest_players(): Set[Player] = {
    val the_others_states = playerstates.filter(playerstate => playerstate.player.id != current)
    the_others_states.map(the_other => the_other.player).toSet
  }
}

// Contains information about each player's current state
//
// * Input
// player
// board : player's current board
case class PlayerState(player: Player, board: PlayerBoard)


// Represents each player
//
// * Input
// id : an int value starting from 0
// color: the player's unique flag color
case class Player(id: Int): 
  import io.AnsiColor._
  
  // To identify ownership of flags
  def get_color(): String = {
    id match {
      case 0 => RED
      case 1 => GREEN
      case 2 => YELLOW
      case 3 => BLUE
      case 4 => MAGENTA
      case 5 => CYAN
      case _ => throw IllegalStateException("Maximum number of players is 6.")
    }
  }


// Provides a way to check validity of an action in combination with the coordinate
// * Input
// action : one of Reveal, Flag, and Unflag
// pos : the tile coordinate
case class PlayerAction(val action: Action, val tile_pos: Coordinate)

enum Action:
  case Reveal, Flag, Unflag


//////////////////////////////
///////// Game Logic /////////
//////////////////////////////
def new_game(difficulty: GameDifficulty): GameState = 
  val mine_board = create_mineboard_for_game(difficulty)
  val solution_board = create_solutionboard(mine_board)
  val player_board = create_playerboard(mine_board.xsize, mine_board.ysize)

  // number of players is fixed as 2 now
  val playerpool = create_playerpool(2, player_board)    
  
  GameState(solution = Solution(solution_board, difficulty.num_mines), 
            playerpool = playerpool,
            status = GameStatus.Continue)
            

// Creates a new player pool for a new game
// Each player is given an id starting from 0, a color, and a new playerboard
def create_playerpool(num_players: Int, playerboard: PlayerBoard): PlayerPool = {
  val players = (0 until num_players).map(n => Player(id = n)).toList
  val playerstates = players.map(player => PlayerState(player, playerboard))

  PlayerPool(playerstates, 0)
}


// Checks whether the game should continue or end
def game_over(status: GameStatus): Boolean = 
  status match {
    case GameStatus.Win(_) => true
    case GameStatus.Continue => false
  }


// Updates the game state by applying changes to the playerpool and the game status base on the player's action 
// 1. playerpool
// - If Reveal: updates only the current player's board
// - If Flag, Unflag: updates all player's boards
// 2. game status (Win or Continue) 
// - If revealed a mine, game ends
// - If flagged all the mines, game ends
// - Otherwise, game continues
def play(state: GameState, player_action: PlayerAction): GameState = 
  val current_player = state.playerpool.current_player()
  
  state.status match {
    // play is only valid in GameStatus.Continue
    case GameStatus.Continue => {
      val updated_playerpool = player_action.action match {
        case Action.Reveal => update_player(state.playerpool, reveal(state.solution.board, player_action.tile_pos))
        case Action.Flag => update_playerpool(state.playerpool, flag(current_player, player_action.tile_pos))
        case Action.Unflag => update_playerpool(state.playerpool, unflag(current_player, player_action.tile_pos))
      }
      val updated_status = update_status(updated_playerpool, player_action.tile_pos, state.solution.board)

      state.copy(playerpool = updated_playerpool, status = updated_status)
    }

    case _ => throw IllegalStateException("You can only play game in Continue Status.")
  }

// Updates the current player's state in the current pool
// Mainly for when the player takes the reveal action which only affects their own board
//
// * Input
// playerpool: current player pool
// update: a function that updates the current player's board; most likely `reveal`
// * Output
// A new playerpool with only the current player's board updated
def update_player(playerpool: PlayerPool, update: PlayerBoard => PlayerBoard): PlayerPool = {
  val current = playerpool.current
  val current_playerstates = playerpool.playerstates
  val current_player = playerpool.current_player()
  val current_playerstate = playerpool.current_playerstate()
  val current_playerboard = playerpool.current_playerboard()
  
  // Return a copy of the payerpool where current player's board is updated by the `update` function
  playerpool.copy(playerstates = current_playerstates.updated(current, current_playerstate.copy(board = update(current_playerboard))))
}


// Updates every player's board in the current playerpool with the update function
// 
// * Input
// playerpool: current playerpool
// update: either flag or unflag
// * Output
// A new playerpool where all players' boards are updated; next index unchanged
def update_playerpool(playerpool: PlayerPool, update: PlayerBoard => Option[PlayerBoard]): PlayerPool = {
  val updated_playerstates = playerpool.playerstates.map(playerstate => 
    update(playerstate.board) match {
      case Some(updated_playerboard) => playerstate.copy(board = updated_playerboard)
      case None => playerstate
    }
  )

  playerpool.copy(playerstates = updated_playerstates)
}


// Computes the new game status by checking if the current player has won or lost
// - If the current player revealed a mine, the rest of the players win
// - If the current player's board satisfies the winning conditon, winners are computed
// - Otherwise, game continues
def update_status(playerpool: PlayerPool, tile_pos: Coordinate, solution_board: SolutionBoard): GameStatus = {
  val current_playerstate = playerpool.current_playerstate()
  
  if revealed_mine(current_playerstate.board, tile_pos) then
    GameStatus.Win(playerpool.get_rest_players())
  else if all_mines_flagged(current_playerstate.board, solution_board) then
    GameStatus.Win(who_won(playerpool, solution_board))
  else
    GameStatus.Continue
}


// Checks if a mine is revealed at the coordinate
def revealed_mine(playerboard: PlayerBoard, pos: Coordinate): Boolean = {
  playerboard.tileMap(pos) match {
    case PlayerTile.Revealed(SolutionTile.Mine) => true
    case PlayerTile.RevealedNFlagged(SolutionTile.Mine, _) => true
    case _ => false
  }
}


// Determine the winners
// Triggered by flagging all mines
// You win if you have the most points
// +1 for a correctly placed flag (on a mine)
// -1 for an incorrectly placed flag
def who_won(playerpool: PlayerPool, solution_board: SolutionBoard): Set[Player] = {
  val players = playerpool.get_players()

  val points_per_player = playerpool.playerstates.map(playerstate => get_points(playerstate, mine_coordinates(solution_board)))
  val max_points = points_per_player.max
  val winners_indices = points_per_player.zipWithIndex.flatMap((points, i) => if points == max_points then Some(i) else None)
  
  winners_indices.map(i => players(i)).toSet
}

// Computes how many flags are correctly and incorrectly placed by a player
// * Input
// playerstate: contains player and playerboard info
// mine_coordinates: set of coordinates where mines are
// * Output
// (number of correctly placed flags, number of incoordctly placed flags)
def num_flagged_mines_by(playerstate: PlayerState, mine_coordinates: Set[Coordinate]): (Int, Int) = {
  val player = playerstate.player 
  val board = playerstate.board 

  val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => board.tileMap(pos) match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_all_flags = board.tileMap.foldRight(0)((x, acc) => x._2 match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_flagged_non_mines = num_all_flags - num_flagged_mines

  (num_flagged_mines, num_flagged_non_mines)
}


// Calculates points for the player
def get_points(playerstate: PlayerState, mine_coordinates: Set[Coordinate]): Int = {
  val player = playerstate.player 
  val board = playerstate.board 

  val num_all_flags = board.tileMap.foldRight(0)((x, acc) => x._2 match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => board.tileMap(pos) match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_flagged_non_mines = num_all_flags - num_flagged_mines


  num_flagged_mines - num_flagged_non_mines
}


// Returns the list of coordinates where mines are
def mine_coordinates(solution_board: SolutionBoard): Set[Coordinate] = {
  solution_board.tileMap.filter((_, s_tile) => s_tile == SolutionTile.Mine).keys.toSet
}


// Checks whether all mines are flagged
// Game is over and winners are decided once all mines are flagged
def all_mines_flagged(player_board: PlayerBoard, solution_board: SolutionBoard): Boolean = {
  val mines = mine_coordinates(solution_board)

  // assumption: set of flagged pos is the same across all playerboards 
  val all_flagged = player_board.tileMap.map(x => x._2 match {
      case PlayerTile.Flagged(_) => Some(x._1) // flagged by this player
      case PlayerTile.RevealedNFlagged(_, _) => Some(x._1)  // flagged by other players
       case _ => None
    }//match
  ).toSet.flatten//inner map ->  Set[Option[Coordinate]]

  mines subsetOf all_flagged
}


// Checks the tile at the coordinate is a mine
def is_mine(pos: Coordinate, solution_board: SolutionBoard): Boolean = {
  mine_coordinates(solution_board).contains(pos) // Set[Option[Coordinate]]
}



// Updates the game state by setting the current player's index to be next
def next_player(state: GameState): GameState = state.copy(playerpool = state.playerpool.next_pool())


// updates players' boards by revealing all mines
// When this is called, the winners have already been decided
def update_with_result(state: GameState): GameState = {
  // 1. update every player's board by revealing all mines
  val all_mines_revealed = state.playerpool.playerstates.map(playerstate => 
    reveal_all_mines(state.solution.board, playerstate.board))
  // 2. update playerstates with the updated boards
  val last_playerstates = state.playerpool.playerstates.map(playerstate => 
    playerstate.copy(board = all_mines_revealed(playerstate.player.id)))
  // 3. update playerpool with the updated playerstates
  val last_playerpool = state.playerpool.copy(playerstates = last_playerstates)
  // 4. lastly, update the state with the updated playerpool
  state.copy(playerpool = last_playerpool)
}



// Notes:
// Separate Game(model) and text ui(view).
// Game - Coordinate || text ui - InputCoordinate
// To clean up, we rebuilt via Metals
// Had nested sbt files 


// // tiles and mines ratio
// enum Difficulty:
//   case Easy //  12.6%
//   case Intermediate // 18.1%
//   case Expert // 20.6%
