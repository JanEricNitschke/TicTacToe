// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#include <tictactoe.h>

#include <algorithm>
#include <array>
#include <chrono>  // NOLINT [build/c++11]
#include <iostream>
#include <random>
#include <string>
#include <thread>  // NOLINT [build/c++11]
#include <tuple>
#include <unordered_map>
#include <vector>

// Initialize an empty game board
auto createBoard() -> GameBoard {
  GameBoard board{{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  return board;
}

// Function to get yes/no response from the player
auto getPlayerYesNo(const std::string question) -> bool {
  std::string solo;
  while (solo != "Y" && solo != "N") {
    std::cout << question << std::endl;
    std::cin >> solo;
    std::transform(solo.begin(), solo.end(), solo.begin(), ::toupper);
  }
  return static_cast<bool>(solo == "Y");
}

// Get information whether it is a
// 1 person or 2 person game
auto getPlayerNumber() -> bool {
  return getPlayerYesNo("Play alone vs AI?[y/n]: ");
}

// Get information whether the AI should make
// the first move.
// Player 'X' makes odd moves so if AI
// should make the first it needs to be player 'X'
auto getAIStart() -> std::string {
  if (getPlayerYesNo("Should the AI make the first move?[y/n]: ")) {
    return "X";
  }
  return "O";
}

// Ask user for AI strength
// 1 is Random
// 2 wins if possible
// 3 wins or blocks if possible
// 4 plays perfect
auto getAIStrength() -> int {
  std::string input;
  int strength;
  std::cout << "AI strength settings:" << std::endl;
  std::cout << "1: Easy" << std::endl;
  std::cout << "2: Medium" << std::endl;
  std::cout << "3: Hard" << std::endl;
  std::cout << "4: Impossible" << std::endl;
  while (true) {
    std::cout << "How strong should the AI be?[1-4]: " << std::endl;
    std::cin >> input;
    try {
      strength = std::stoi(input);
    } catch (std::invalid_argument const &ex) {
      std::cout << "Invalid input" << std::endl;
      continue;
    }
    if (strength > 4 || strength < 1) {
      std::cout << "Invalid input" << std::endl;
      continue;
    }
    break;
  }
  return strength;
}

// Checks if the given player
// has won on the given board
auto isPlayerWin(const std::string &player, const GameBoard &board) -> bool {
  bool win{false};
  const int board_size = board.size();

  // checking rows
  for (int i = 0; i < board_size; i++) {
    win = true;
    for (int j = 0; j < board_size; j++) {
      if (board[i][j] != player) {
        win = false;
        break;
      }
    }
    if (win) {
      return win;
    }
  }

  // checking cols
  for (int i = 0; i < board_size; i++) {
    win = true;
    for (int j = 0; j < board_size; j++) {
      if (board[j][i] != player) {
        win = false;
        break;
      }
    }
    if (win) {
      return win;
    }
  }

  // checking diagonal
  win = true;
  for (int i = 0; i < board_size; i++) {
    if (board[i][i] != player) {
      win = false;
      break;
    }
  }
  if (win) {
    return win;
  }

  // checking antidiagonal
  win = true;
  for (int i = 0; i < board_size; i++) {
    if (board[i][board_size - i - 1] != player) {
      win = false;
      break;
    }
  }
  if (win) {
    return win;
  }

  // found no line of 3
  return false;
}

// Checks if the board is completely filled
// That is the case if no '-' can be found
// Will be used to check for draw after checking
// for either player win first
auto isBoardFilled(const GameBoard &board) -> bool {
  const int board_size = board.size();
  for (int i = 0; i < board_size; i++) {
    for (int j = 0; j < board_size; j++) {
      if (board[j][i] == "-") {
        return false;
      }
    }
  }
  return true;
}

// Swap between player X and O
// Only expected X or O as input
std::string swapPlayer(const std::string &player) {
  return (player == "X") ? "O" : "X";
}

std::vector<std::array<int, 2>> getEmptyCells(const GameBoard &board) {
  const int board_size = board.size();
  std::vector<std::array<int, 2>> empty_cells;
  for (int i = 0; i < board_size; i++) {
    for (int j = 0; j < board_size; j++) {
      if (board[i][j] == "-") {
        empty_cells.push_back({{i, j}});
      }
    }
  }
  return empty_cells;
}

// Performs any random valid move
Move randomMove(const GameBoard &board) {
  // Get all the empty cells
  const std::vector<std::array<int, 2>> empty_cells{getEmptyCells(board)};
  // Pick a random number in range 0, length of the list and take that element
  std::array<int, 2> chosenCell = empty_cells[std::rand() % empty_cells.size()];
  return {{chosenCell[0], chosenCell[1], 0}};
}

// Adjust wincondition requirements according to board state.
void checkWinconditions(
    const std::string &player, const GameBoard &board,
    std::unordered_map<std::string, std::set<std::tuple<int, int>>>
        *win_conditions) {
  const int board_size = board.size();
  for (int row = 0; row < board_size; row++) {
    for (int col = 0; col < board_size; col++) {
      // If the given player occupies this cell
      // then that reduces the required positions
      // in that line by one
      if (board[row][col] == player) {
        if ((*win_conditions)["row" + std::to_string(row)].find(
                std::make_tuple(row, col)) !=
            (*win_conditions)["row" + std::to_string(row)].end()) {
          (*win_conditions)["row" + std::to_string(row)].erase(
              std::make_tuple(row, col));
        }
        if ((*win_conditions)["col" + std::to_string(col)].find(
                std::make_tuple(row, col)) !=
            (*win_conditions)["col" + std::to_string(col)].end()) {
          (*win_conditions)["col" + std::to_string(col)].erase(
              std::make_tuple(row, col));
        }
        if (row == col && (*win_conditions)["diag"].find(std::make_tuple(
                              row, col)) != (*win_conditions)["diag"].end()) {
          (*win_conditions)["diag"].erase(std::make_tuple(row, col));
        }
        if (row == (board_size - 1 - col) &&
            (*win_conditions)["antidiag"].find(std::make_tuple(row, col)) !=
                (*win_conditions)["antidiag"].end()) {
          (*win_conditions)["antidiag"].erase(std::make_tuple(row, col));
        }
      }
      // If the opposing player occupies this cell
      // then all lines that contain it become useless
      if (board[row][col] == swapPlayer(player)) {
        (*win_conditions)["row" + std::to_string(row)].clear();
        (*win_conditions)["col" + std::to_string(col)].clear();
        if (row == col) {
          (*win_conditions)["diag"].clear();
        }
        if (row == (board_size - 1 - col)) {
          (*win_conditions)["antidiag"].clear();
        }
      }
    }
  }
}

// Tries to find a move where the given player wins on the
// given board. So any line that contains the player twice
// and an empty cell as the last slot
Move getWinningMove(const std::string &player, const GameBoard &board) {
  // Build  all of the possible lines
  std::unordered_map<std::string, std::set<std::tuple<int, int>>>
      win_conditions{
          {"row0", std::set<std::tuple<int, int>>({std::make_tuple(0, 0),
                                                   std::make_tuple(0, 1),
                                                   std::make_tuple(0, 2)})},
          {"row1", std::set<std::tuple<int, int>>({std::make_tuple(1, 0),
                                                   std::make_tuple(1, 1),
                                                   std::make_tuple(1, 2)})},
          {"row2", std::set<std::tuple<int, int>>({std::make_tuple(2, 0),
                                                   std::make_tuple(2, 1),
                                                   std::make_tuple(2, 2)})},
          {"col0", std::set<std::tuple<int, int>>({std::make_tuple(0, 0),
                                                   std::make_tuple(1, 0),
                                                   std::make_tuple(2, 0)})},
          {"col1", std::set<std::tuple<int, int>>({std::make_tuple(0, 1),
                                                   std::make_tuple(1, 1),
                                                   std::make_tuple(2, 1)})},
          {"col2", std::set<std::tuple<int, int>>({std::make_tuple(0, 2),
                                                   std::make_tuple(1, 2),
                                                   std::make_tuple(2, 2)})},
          {"diag", std::set<std::tuple<int, int>>({std::make_tuple(0, 0),
                                                   std::make_tuple(1, 1),
                                                   std::make_tuple(2, 2)})},
          {"antidiag", std::set<std::tuple<int, int>>(
                           {std::make_tuple(0, 2), std::make_tuple(1, 1),
                            std::make_tuple(2, 0)})}};
  // Adjust wincondition requirements according to board state.
  checkWinconditions(player, board, &win_conditions);
  // Check if any line requires exactly one more
  // position from the player to be fulfilled
  for (auto const &x : win_conditions) {
    if (x.second.size() == 1) {
      std::tuple<int, int> firstWin = *x.second.begin();
      Move winMove = {{std::get<0>(firstWin), std::get<1>(firstWin), 0}};
      return winMove;
    }
  }
  // If there is no winning move return a default value
  return {{-1, -1, -1}};
}

// Tries to find a move that would block the opponent
// winning on their next move
Move getBlockingMove(const std::string &player, const GameBoard &board) {
  // Just find a move that would make the opponent win
  return getWinningMove(swapPlayer(player), board);
}

// Try to perform a winning move
// If there is none return a random one instead
Move winMove(const std::string &player, const GameBoard &board) {
  Move winMove = getWinningMove(player, board);
  if (winMove[2] == 0) {
    return winMove;
  }
  return randomMove(board);
}

// Try to find a winning or blocking move
// If neither exists do a random one instead
Move blockWinMove(const std::string &player, const GameBoard &board) {
  Move winMove = getWinningMove(player, board);
  if (winMove[2] == 0) {
    return winMove;
  }
  Move blockMove = getBlockingMove(player, board);
  if (blockMove[2] == 0) {
    return blockMove;
  }
  return randomMove(board);
}
// Takes a board state and returns the coordinates of the optimal move for the
// given player
Move minmax(const std::string &player, GameBoard *board) {
  // Base cases
  // Player won
  Move best_move{{-1, -1, -1}};
  if (isPlayerWin(player, *board)) {
    best_move[2] = 1;
    return best_move;
  }
  // Player lost
  if (isPlayerWin(swapPlayer(player), *board)) {
    best_move[2] = -1;
    return best_move;
  }
  const std::vector<std::array<int, 2>> empty_cells = getEmptyCells(*board);
  // Game is drawn
  if (empty_cells.empty()) {
    best_move[2] = 0;
    return best_move;
  }
  // To reduce required computation
  // and increase replayability
  // just do a random move as the first
  // Optimal play still forces a draw
  const int board_size = board->size();
  if (empty_cells.size() == (board_size * board_size)) {
    best_move[0] = std::rand() % board_size;
    best_move[1] = std::rand() % board_size;
    best_move[2] = 0;
    return best_move;
  }
  // Recursively apply minmax algorithm
  for (const std::array<int, 2> &cell : empty_cells) {
    (*board)[cell[0]][cell[1]] = player;
    Move currentMove = minmax(swapPlayer(player), board);
    if (-currentMove[2] > best_move[2]) {
      best_move = {{cell[0], cell[1], -currentMove[2]}};
    }
    (*board)[cell[0]][cell[1]] = "-";
  }
  return best_move;
}

// Pretty print the current board
void showBoard(const GameBoard &board) {
  std::string line_separator = "---------------";
  const int board_size = board.size();
  std::cout << line_separator << std::endl;
  for (int i = 0; i < board_size; i++) {
    for (int j = 0; j < board_size; j++) {
      std::cout << "| " << board[i][j] << " |";
    }
    std::cout << std::endl << line_separator << std::endl;
  }
}

// Perform AI move
void aiTurn(const std::string &player, GameBoard *board, int ai_strength) {
  // Inform the player of the game state
  std::cout << "AI turn as player " << player << "." << std::endl;
  showBoard(*board);
  Move best_move;
  // Check which function to use to perform AI move
  switch (ai_strength) {
    // False positive
    case 1:
      best_move = randomMove(*board);
      break;
    case 2:
      best_move = winMove(player, *board);
      break;
    case 3:
      best_move = blockWinMove(player, *board);
      break;
    default:
      best_move = minmax(player, board);
  }
  // Perform the move
  (*board)[best_move[0]][best_move[1]] = player;
  // Wait 1 second to have a smooth playing experience
  std::this_thread::sleep_for(std::chrono::seconds(1));
}

// Perform player turn
void playerTurn(const std::string &player, GameBoard *board) {
  int row{-1};
  int col{-1};
  bool valid_move{false};
  // Let the player input their move and validate
  while (!valid_move) {
    std::cout << "Player " << player << " turn" << std::endl;
    showBoard(*board);
    valid_move = true;
    std::string input1;
    std::string input2;
    std::cout << "Enter row and column numbers to fix spot: " << std::endl;
    std::cin >> input1 >> input2;
    // That they entered two numbers
    try {
      row = std::stoi(input1);
      col = std::stoi(input2);
    } catch (std::invalid_argument const &ex) {
      std::cout << "At least one of your entered inputs of (" << input1 << ", "
                << input2
                << ") could not be converted to an integer. Try again!"
                << std::endl;
      valid_move = false;
      continue;
    }
    // That the numbers are within bounds
    if (row > 3 || row < 1 || col > 3 || col < 1) {
      std::cout << "Row " << row << " or column " << col
                << " are out of bounds. They have to be between 1 and 3 "
                   "inclusive. Try again!"
                << std::endl;
      valid_move = false;
      continue;
    }
    // And that the position is not taken
    if ((*board)[row - 1][col - 1] != "-") {
      std::cout << "The position (" << row << ", " << col
                << ") has already been taken by a player! Please do your move "
                   "on an empty position."
                << std::endl;
      valid_move = false;
      continue;
    }
  }
  (*board)[row - 1][col - 1] = player;
}
