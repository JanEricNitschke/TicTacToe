// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#ifndef TICTACTOE_CPP_INCLUDE_TICTACTOE_H_
#define TICTACTOE_CPP_INCLUDE_TICTACTOE_H_

#include <Random.h>

#include <array>
#include <iostream>
#include <limits>
#include <set>
#include <string>
#include <string_view>
#include <thread>  // NOLINT [build/c++11]
#include <tuple>
#include <unordered_map>
#include <vector>

// Yes, this could just be a vector and then we could set the
// size at runtime too. But i wanted to do something with templates.
template <std::size_t N>
using GameBoard = std::array<std::array<char, N>, N>;
// // Define aliases for the data structures
// // used for Move and TicTacToeBoard as they are common
// // and long
using TicTacToeBoard = GameBoard<3>;

enum class GameState {
  undecided = 0,
  loss = 1,
  draw = 2,
  win = 3,
};

GameState operator-(GameState const &state);

std::ostream &operator<<(std::ostream &os, const GameState &obj);

struct Spot {
  std::size_t row{};
  std::size_t col{};

  bool operator==(const Spot &other) const {
    return (row == other.row && col == other.col);
  }
};

struct Move {
  Spot spot{};
  GameState state{};

  bool operator==(const Move &other) const {
    return (spot == other.spot && state == other.state);
  }
};

struct AISettings {
  bool isAI{};
  int strength{};
};

struct BestMoves {
  std::vector<Spot> spots{};
  GameState state{};
};

template <std::size_t N>
// Initialize an empty game board
constexpr auto createBoard() -> GameBoard<N> {
  GameBoard<N> board{};
  for (auto &row : board) {
    for (auto &col : row) {
      col = '-';
    }
  }
  return board;
}

// Get AI settings for the given player
AISettings getAISettings(char player);

// Function to get yes/no response from the player
bool getPlayerYesNo(std::string_view question);

// Get information whether it is a
// 1 person or 2 person game
bool getPlayerNumber();

// Get information whether the AI should make
// the first move.
// Player 'X' makes odd moves so if AI
// should make the first it needs to be player 'X'
char getAIStart();

// Ask user for AI strength
// 1 is Random
// 2 wins if possible
// 3 wins or blocks if possible
// 4 plays perfect
int getAIStrength();

// Checks if the given player
// has won on the given board
// Checks if the given player
// has won on the given board
template <std::size_t N>
auto isPlayerWin(char player, const GameBoard<N> &board) -> bool {
  bool win{false};

  // checking rows
  for (size_t i{0}; i < N; i++) {
    win = true;
    for (size_t j{0}; j < N; j++) {
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
  for (size_t i{0}; i < N; i++) {
    win = true;
    for (size_t j{0}; j < N; j++) {
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
  for (size_t i{0}; i < N; i++) {
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
  for (size_t i{0}; i < N; i++) {
    if (board[i][N - i - 1] != player) {
      win = false;
      break;
    }
  }
  if (win) {
    return win;
  }

  // found no full line
  return false;
}

// Checks if the board is completely filled
// That is the case if no '-' can be found
// Will be used to check for draw after checking
// for either player win first
template <std::size_t N>
auto isBoardFilled(const GameBoard<N> &board) -> bool {
  for (size_t i{0}; i < N; i++) {
    for (size_t j{0}; j < N; j++) {
      if (board[j][i] == '-') {
        return false;
      }
    }
  }
  return true;
}

// Swap between player X and O
// Only expected X or O as input
char swapPlayer(char player);

// Get all currently unoccupied cells
// Used for random move and minmax
template <std::size_t N>
std::vector<Spot> getEmptyCells(const GameBoard<N> &board) {
  std::vector<Spot> empty_cells{};
  for (size_t i{0}; i < N; i++) {
    for (size_t j{0}; j < N; j++) {
      if (board[i][j] == '-') {
        empty_cells.emplace_back(i, j);
      }
    }
  }
  return empty_cells;
}

// Performs any random valid move
template <std::size_t N>
Move randomMove(const GameBoard<N> &board) {
  // Get all the empty cells
  const std::vector<Spot> empty_cells{getEmptyCells(board)};
  // Pick a random number in range 0, length of the list and take that element
  Spot chosenCell{
      empty_cells[Random::get<std::size_t>(0, empty_cells.size() - 1)]};
  return {.spot = {.row{chosenCell.row}, .col{chosenCell.col}},
          .state{GameState::undecided}};
}

// Adjust wincondition requirements according to board state.
template <std::size_t N>
void checkWinconditions(
    char player, const GameBoard<N> &board,
    std::unordered_map<std::string,
                       std::set<std::tuple<std::size_t, std::size_t>>>
        *win_conditions) {
  for (size_t row{0}; row < N; row++) {
    for (size_t col{0}; col < N; col++) {
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
        if (row == (N - 1 - col) &&
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
        if (row == (N - 1 - col)) {
          (*win_conditions)["antidiag"].clear();
        }
      }
    }
  }
}

// Tries to find a move where the given player wins on the
// given board. So any line that contains the player twice
// and an empty cell as the last slot
template <std::size_t N>
Move getWinningMove(char player, const GameBoard<N> &board) {
  // Build  all of the possible lines
  std::unordered_map<std::string,
                     std::set<std::tuple<std::size_t, std::size_t>>>
      win_conditions{};
  for (size_t row{0}; row < N; row++) {
    for (size_t col{0}; col < N; col++) {
      win_conditions["row" + std::to_string(row)].insert(
          std::make_tuple(row, col));
      win_conditions["col" + std::to_string(col)].insert(
          std::make_tuple(row, col));
      if (row == col) {
        win_conditions["diag"].insert(std::make_tuple(row, col));
      }
      if (row == (N - 1 - col)) {
        win_conditions["antidiag"].insert(std::make_tuple(row, col));
      }
    }
  }
  // Adjust wincondition requirements according to board state.
  checkWinconditions(player, board, &win_conditions);
  // Check if any line requires exactly one more
  // position from the player to be fulfilled
  for (const auto &x : win_conditions) {
    if (x.second.size() == 1) {
      std::tuple<std::size_t, std::size_t> firstWin{*x.second.begin()};
      return {
          .spot = {.row{std::get<0>(firstWin)}, .col{std::get<1>(firstWin)}},
          .state{GameState::win}};
    }
  }
  // If there is no winning move return a default value
  return {.spot = {.row{0}, .col{0}}, .state{GameState::undecided}};
}

// Tries to find a move that would block the opponent
// winning on their next move
template <std::size_t N>
Move getBlockingMove(char player, const GameBoard<N> &board) {
  // Just find a move that would make the opponent win
  return getWinningMove(swapPlayer(player), board);
}

// Try to perform a winning move
// If there is none return a random one instead
template <std::size_t N>
Move winMove(char player, const GameBoard<N> &board) {
  Move winMove{getWinningMove(player, board)};
  if (winMove.state != GameState::undecided) {
    return winMove;
  }
  return randomMove(board);
}

// Try to find a winning or blocking move
// If neither exists do a random one instead
template <std::size_t N>
Move blockWinMove(char player, const GameBoard<N> &board) {
  Move winMove{getWinningMove(player, board)};
  if (winMove.state != GameState::undecided) {
    return winMove;
  }
  Move blockMove{getBlockingMove(player, board)};
  if (blockMove.state != GameState::undecided) {
    return blockMove;
  }
  return randomMove(board);
}

// The coordinates of the optimal moves for the player on the board
template <std::size_t N>
BestMoves getBestMoves(char player, GameBoard<N> *board) {
  // Base cases
  // Player won
  BestMoves best_moves{.spots{}, .state = GameState::undecided};
  if (isPlayerWin(player, *board)) {
    best_moves.state = GameState::win;
    return best_moves;
  }
  // Player lost
  if (isPlayerWin(swapPlayer(player), *board)) {
    best_moves.state = GameState::loss;
    return best_moves;
  }
  const std::vector<Spot> empty_cells{getEmptyCells(*board)};
  // Game is drawn
  if (empty_cells.empty()) {
    best_moves.state = GameState::draw;
    return best_moves;
  }
  // To reduce required computation
  // and increase replayability
  // just do a random move as the first
  // Optimal play still forces a draw
  if (empty_cells.size() == (N * N)) {
    Move random_move{randomMove(*board)};
    best_moves.spots.push_back(random_move.spot);
    return best_moves;
  }
  // Recursively apply minmax algorithm
  for (const auto &cell : empty_cells) {
    (*board)[cell.row][cell.col] = player;
    BestMoves current_moves{getBestMoves(swapPlayer(player), board)};
    if (-current_moves.state > best_moves.state) {
      best_moves.state = -current_moves.state;
      best_moves.spots = {cell};
    } else if (-current_moves.state == best_moves.state) {
      best_moves.spots.push_back(cell);
    }
    (*board)[cell.row][cell.col] = '-';
  }
  return best_moves;
}

// The coordinates of the one optimal move for the player on the board
template <std::size_t N>
Move minmax(char player, GameBoard<N> *board) {
  BestMoves best_moves{getBestMoves(player, board)};
  if (best_moves.spots.empty()) {
    return {.spot{}, .state{best_moves.state}};
  }
  Spot random_best_spot =
      best_moves
          .spots[Random::get<std::size_t>(0, best_moves.spots.size() - 1)];
  return {.spot{random_best_spot}, .state{best_moves.state}};
}

// Pretty print the current board
template <std::size_t N>
void showBoard(const GameBoard<N> &board) {
  std::string line_separator(N * 5, '-');
  std::cout << line_separator << std::endl;
  for (size_t i{0}; i < N; i++) {
    for (size_t j{0}; j < N; j++) {
      std::cout << "| " << board[i][j] << " |";
    }
    std::cout << std::endl << line_separator << std::endl;
  }
}

// Perform AI move
template <std::size_t N>
void aiTurn(char player, GameBoard<N> *board, int ai_strength) {
  // Inform the player of the game state
  std::cout << "AI turn as player " << player << "." << std::endl;
  showBoard(*board);
  Move best_move{};
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
  (*board)[best_move.spot.row][best_move.spot.col] = player;
  // Wait 1 second to have a smooth playing experience
  std::this_thread::sleep_for(std::chrono::seconds(1));
}

// Perform player turn
template <std::size_t N>
void playerTurn(char player, GameBoard<N> *board) {
  std::size_t row{};
  std::size_t col{};
  bool valid_move{false};
  // Let the player input their move and validate
  while (!valid_move) {
    std::cout << "Player " << player << " turn" << std::endl;
    showBoard(*board);
    valid_move = true;
    std::string input1{};
    std::string input2{};
    std::cout << "Enter row and column numbers to fix spot: " << std::endl;
    std::cin >> input1 >> input2;
    // That they entered two numbers
    try {
      row = std::stoull(input1);
      col = std::stoull(input2);
    } catch (std::invalid_argument const &ex) {
      std::cout << "At least one of your entered inputs of (" << input1 << ", "
                << input2
                << ") could not be converted to an integer. Try again!"
                << std::endl;
      valid_move = false;
      std::cin.clear();  // Clear the error state
      std::cin.ignore(std::numeric_limits<std::streamsize>::max(),
                      '\n');  // Clear input buffer
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
    if ((*board)[row - 1][col - 1] != '-') {
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
#endif  // TICTACTOE_CPP_INCLUDE_TICTACTOE_H_
