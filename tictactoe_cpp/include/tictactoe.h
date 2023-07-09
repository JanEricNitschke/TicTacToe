#ifndef TICTACTOE_H
#define TICTACTOE_H
#include <tictactoe.h>
#include <iostream>
#include <array>
#include <vector>

// // Define aliases for the data structures
// // used for Move and GameBoard as they are common
// // and long
typedef std::array<std::array<std::string, 3>, 3>
    GameBoard;
typedef std::array<int, 3> Move;

// Initialize an empty game board
GameBoard createBoard();

// Function to get yes/no response from the player
bool getPlayerYesNo(const std::string question);

// Get information whether it is a
// 1 person or 2 person game
bool getPlayerNumber();

// Get information whether the AI should make
// the first move.
// Player 'X' makes odd moves so if AI
// should make the first it needs to be player 'X'
std::string getAIStart();

// Ask user for AI strength
// 1 is Random
// 2 wins if possible
// 3 wins or blocks if possible
// 4 plays perfect
int getAIStrength();

// Checks if the given player
// has won on the given board
bool isPlayerWin(const std::string &player, const GameBoard &board);

// Checks if the board is completely filled
// That is the case if no '-' can be found
// Will be used to check for draw after checking
// for either player win first
bool isBoardFilled(const GameBoard &board);

// Swap between player X and O
// Only expected X or O as input
std::string swapPlayer(const std::string &player);

std::vector<std::array<int, 2>> getEmptyCells(const GameBoard &board);

// Performs any random valid move
Move randomMove(GameBoard &board);

// Tries to find a move where the given player wins on the
// given board. So any line that contains the player twice
// and an empty cell as the last slot
Move getWinningMove(const std::string &player, GameBoard &board);

// Tries to find a move that would block the opponent
// winning on their next move
Move getBlockingMove(const std::string &player, GameBoard &board);

// Try to perform a winning move
// If there is none return a random one instead
Move winMove(const std::string &player, GameBoard &board);

// Try to find a winning or blocking move
// If neither exists do a random one instead
Move blockWinMove(const std::string &player, GameBoard &board);

// Takes a board state and returns the coordinates of the optimal move for the given player
Move minmax(const std::string &player, GameBoard &board);

// Pretty print the current board
void showBoard(const GameBoard &board);

// Perform AI move
void aiTurn(const std::string &player, GameBoard &board, int AI_strength);

// Perform player turn
void playerTurn(const std::string &player, GameBoard &board);
#endif
