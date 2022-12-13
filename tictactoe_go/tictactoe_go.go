/*
tictactoe_go lets you play TicTacToe.
You can play in two player mode with a friend.
Alternatively you can play alone vs an AI
that utilizes the minmax algorithm to play optimally.

Usage:

	tictactoe_go
*/
package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strings"
	"time"
)

func main() {
	tictactoe := NewTicTacToe()
	tictactoe.GetSettings()
	tictactoe.Play()
}

// Board is as an alias for the 3x3 array of runes used to represent game board.
type Board = [3][3]rune

// A TicTacToe represents a game of tictactoe
// with a board as well as settings regarding an
// AI opponent.
type TicTacToe struct {
	// Contains the given board state of the game
    board Board
    /// Tracks whether it is one person playing vs AI or not
    aiOpponent bool
    /// Hold the char that the AI is playing as
    aiPlayer rune
}

// A Move represents a move of tictactoe with its
// position and expected end state.
type Move struct {
	// endState attribute for the outcome of this move given perfect follow up play by both sides.
    endState int
	// Row and col attributes for the position where the move should be made.
    // Integers expected to be in the range 0..=2.
	row int
    col int
}

// showBoard prints the board of the received TicTacToe game
// to stdout wiht the pretty formatting.
func (tictactoe *TicTacToe) showBoard() {
	// Separator for the rows of the board
	var strLine = "---------------";

	fmt.Println(strLine);
	for row := 0; row < int(len(tictactoe.board)); row++ {
		for col := 0; col < int(len(tictactoe.board[0])); col++ {
			fmt.Printf("| %c |", tictactoe.board[row][col]);
		}
		fmt.Printf("\n%s\n", strLine)
	}
}

// GetSettings acquires settings related to the AI
// opponent from the user via stdin
func (tictactoe *TicTacToe) GetSettings() {
	tictactoe.getAiOpponentExists()
	if tictactoe.aiOpponent {
		tictactoe.getAiOpponentStart()
	}
}

// getAIOpponentExists queries the user for whether
// they want to play alone of with a friend.
func (tictactoe *TicTacToe) getAiOpponentExists() {
	for {
		// Will hold the user input
		var opponent string
		fmt.Print("Play alone vs AI?[y/n] ");
		_, err := fmt.Scanln(&opponent)
		if err != nil {
			if err.Error() != "unexpected newline" {
			bufio.NewReader(os.Stdin).ReadString('\n')
			}
			continue
		}
		opponent = strings.ToUpper(opponent)
		if opponent == "Y" {
			tictactoe.aiOpponent = true;
			break
		}
		if opponent == "N" {
			tictactoe.aiOpponent = false;
			break
		}
	}
}

// getAIOpponentExists queries the user for whether
// the AI opponent should make the first move.
func (tictactoe *TicTacToe) getAiOpponentStart() {
	for {
		// Will hold the user input
		var opponent string
		fmt.Print("Should the AI make the first move?[y/n] ");
		_, err := fmt.Scanln(&opponent)
		if err != nil {
			if err.Error() != "unexpected newline" {
			bufio.NewReader(os.Stdin).ReadString('\n')
			}
			continue
		}
		opponent = strings.ToUpper(opponent)
		if opponent == "Y" {
			tictactoe.aiPlayer = 'X';
			break
		}
		if opponent == "N" {
			tictactoe.aiPlayer = 'O';
			break
		}
	}
}

// Play starts the game of TicTacToe initializing the player
// and performing the game loop.
func (tictactoe *TicTacToe) Play() {
	// The rune representing the currently active player
	var player = 'X'
	for {
		if tictactoe.aiOpponent && tictactoe.aiPlayer == player {
			tictactoe.aiTurn(player)
		} else {
			tictactoe.playerTurn(player);
		}
		if tictactoe.isPlayerWin(player) {
			fmt.Printf("Player %c wins the game!\n", player);
			break;
		}
		if tictactoe.isBoardFilled() {
			fmt.Println("Match Draw!");
			break;
		}
	player = tictactoe.swapPlayer(player);
	}
	tictactoe.showBoard();
}

// Functionality for performing an AI turn
func (tictactoe *TicTacToe) aiTurn(player rune) {
	fmt.Printf("AI turn as %c.\n", player);
	tictactoe.showBoard();
	bestMove := tictactoe.minmax(player);
	tictactoe.board[bestMove.row][bestMove.col] = player;
	time.Sleep(time.Second)
}

// Iterates over the received TicTacToe's board
// and collects the indices of all empty cells.
func (tictactoe *TicTacToe) getEmptyCells() [][2]int {
	emptyCells := [][2]int{}
	for row := 0; row < int(len(tictactoe.board)); row++ {
		for col := 0; col < int(len(tictactoe.board[0])); col++ {
			if tictactoe.board[row][col] == '-' {
				emptyCells = append(emptyCells, [2]int{row,col})
			}
		}
	}
	return emptyCells
}

// Minmax determines the best possible move for
// the passed player given the board state of
// the received TicTacToe game.
//
// Its does so by utilizing the minmax algorithm
// over alternatingly performing both every possible
// move for each player and checking the resulting
// outcome. The move that results in the best worst
// outcome is chosen.
func (tictactoe *TicTacToe) minmax(player rune) Move {
	var bestMove = Move {
		endState: -1,
		row: -1,
		col: -1,
	};
	// Base-cases of the recursion
	// If the current player has won
	if tictactoe.isPlayerWin(player) {
		bestMove.endState = 1;
		return bestMove;
	}
	// If the current player has lost
	if tictactoe.isPlayerWin(tictactoe.swapPlayer(player)) {
		bestMove.endState = -1;
		return bestMove;
	}
	var emptyCells = tictactoe.getEmptyCells();
	// If the game ended in a draw.
	if len(emptyCells) == 0 {
		bestMove.endState = 0;
		return bestMove;
	}
	// Do not perform the minmax algorithm
	// for an empty board.
	// Instead do a random move.
	// This reduces the computation time
	// and increases replayability.
	// The game quality is also not hurt
	// as the AI will always play a game to a
	// draw at worst.
	if len(emptyCells) == 9 {
		rand.Seed(time.Now().UnixNano())
		bestMove = Move {
			endState: 0,
			row: rand.Intn(3),
			col: rand.Intn(3),

		};
		return bestMove;
	}
	// Perform the recursive evaluation of the
	// minmax algorithm.
	for _, cell := range emptyCells {
		row := cell[0]
		col := cell[1]
		tictactoe.board[row][col] = player;
		currentMove := tictactoe.minmax(tictactoe.swapPlayer(player));
		if -currentMove.endState >= bestMove.endState {
			bestMove = Move {
				endState: -currentMove.endState,
				row: row,
				col: col,
			};
		}
		tictactoe.board[row][col] = '-'
	}
	return bestMove
}

// Logic for a real player's turn.
func (tictactoe *TicTacToe) playerTurn(player rune) {

    fmt.Printf("Player %c turn.\n", player);

	tictactoe.showBoard();
	var row, col int
	for {
		fmt.Print("Enter row and column number to fix spot: ")
		// Get user input
		_, err := fmt.Scanln(&row, &col)
		if err != nil {
			if err.Error() != "unexpected newline" {
			bufio.NewReader(os.Stdin).ReadString('\n')
			}
			continue
		}
		if tictactoe.fixSpot(player, row-1, col-1) {
			break
		}
	}
}

// fixSpot tries to fix the given spot to the given player.
//
// If the coordinates are out of bounds or already used
// then no action is performed as false is returned.
// For valid coordinates the board is modified accordingly
// and true is returned.
func (tictactoe *TicTacToe) fixSpot(player rune, row int, col int)  bool {
        if row >= 3 || row < 0 || col >= 3 || col < 0 {
            fmt.Printf("Row %d or column %d are out of bounds. They have to be between 1 and 3 inclusive. Try again!\n", row+1, col+1)
            return false
        }
        if tictactoe.board[row][col] != '-' {
            fmt.Printf("The position (%d, %d) has already been taken by a player! Please do your move on an empty position.\n", row+1, col+1)
            return false
        }
        tictactoe.board[row][col] = player
        return true
    }

// isBoardFilled checks if the whole board has been
// filled.
//
// If this is performed after relevant isPlayerWin
// then it indicates a draw.
func (tictactoe *TicTacToe) isBoardFilled() bool {
	for row := range tictactoe.board {
		for col := range tictactoe.board[0] {
			if tictactoe.board[row][col] == '-' {
				return false
			}
		}
	}
	return true
}

// isPlayerWin checks if the given player has one the game.
//
// It does so if any row, column or diagonal has
// three entries matching the given player.
func (tictactoe *TicTacToe) isPlayerWin(player rune) bool {
	rowsMap := make(map[int]int)
	colsMap := make(map[int]int)
	diagsMap := make(map[int]int)
	for row := range tictactoe.board {
		for col := range tictactoe.board[0] {
			if tictactoe.board[row][col] == player {
				// Player shows up +1 time in this row
				rowsMap[row] += 1
				colsMap[col] += 1
				if row == col {
					diagsMap[0] += 1
				}
				if row == (len(tictactoe.board) - col - 1) {
					diagsMap[1] += 1
				}
			}
		}
	}
	// Check if the player has 3 entries in any row
	for _, value := range rowsMap {
		if value == 3 {
			return true
		}
	}
	// or column
	for _, value := range colsMap {
		if value == 3 {
			return true
		}
	}
	// or diagonal
	for _, value := range diagsMap {
		if value == 3 {
			return true
		}
	}
	// if not he has not won the game
	return false
}

// swapPlayer returns the non-given player.
//
// Will only be called with 'O' and 'X' and returns
// the opposite.
// For any other input it just returns 'X'.
func (tictactoe *TicTacToe) swapPlayer(player rune) rune {
	if player == 'X' {
		return 'O'
	}
	return 'X'
}

// NewTicTacToe returns a pointer to a fresh TicTacToe
//
// A fresh game has a completely empty board and is by default
// played with two real players.
func NewTicTacToe() *TicTacToe {
    return &TicTacToe{Board{{'-', '-', '-'}, {'-', '-', '-'},{'-', '-', '-'}}, false, 'X'}
}


