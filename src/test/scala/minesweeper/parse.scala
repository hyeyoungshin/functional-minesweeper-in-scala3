package minesweeper

import org.scalatest.funsuite.AnyFunSuite
import os.move
import minesweeper.game.*

class Parse extends AnyFunSuite {

  test("parse_gameaction") {
        val filename = "src/test/json/tcp/gameaction.json"
        val parsed_message = parse_game_message(filename)

        parsed_message match {
          case GameMessage.GameAction(tile_pos, action) => assert(action == "R")
          case _ => fail("Not GameAction!")
      }
  }

  test("parse_gameboard") {
        val filename = "src/test/json/tcp/gameboard.json"
        val parsed_message = parse_game_message(filename)

        parsed_message match {
          case GameMessage.GameBoard(width, height, tile_matrix) => assert(height == 2)
          case _ => fail("Not GameBoard!")
        }
  }

  test("parse_tile_matrix_2x2") {
        val filename = "src/test/json/tcp/gameboard.json"
        val parsed_message = parse_game_message(filename)

        val parsed_board = parsed_message match {
          case GameMessage.GameBoard(_, _, _) => parse_game_board(parsed_message)
          case _ => fail("Not GameBoard!")

        }

        parsed_board match {
          case Right(board) => println(board.tileMap); assert(board.tileMap(Coordinate(0,0)) == PlayerTile.Hidden)
          case Left(msg) => fail(msg)
        }
  }

  test("parse_tile_matrix_3x3") {
        val filename = "src/test/json/tcp/gameboard2.json"
        val parsed_message = parse_game_message(filename)

        val parsed_board = parsed_message match {
          case GameMessage.GameBoard(_, _, _) => parse_game_board(parsed_message)
          case _ => fail("Not GameBoard!")

        }
        //    0   1   2
        // 0 [1][F(0)][x]
        // 1 [ ][0|0] [ ]
        // 2 [ ][F(1)][1]
        parsed_board match {
          case Right(board) => println(board.tileMap); assert(board.tileMap(Coordinate(1,2)) == PlayerTile.Flagged(Player(1)))
          case Left(msg) => fail(msg)
        }
  }
  
  
}