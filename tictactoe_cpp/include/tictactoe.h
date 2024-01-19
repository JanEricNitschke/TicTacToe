// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#ifndef TICTACTOE_CPP_INCLUDE_TICTACTOE_H_
#define TICTACTOE_CPP_INCLUDE_TICTACTOE_H_

#include <algorithm>
#include <array>
#include <iostream>
#include <limits>
#include <string>
#include <string_view>
#include <thread>  // NOLINT [build/c++11]
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Random.h"

// Yes, this could just be a vector and then we could set the
// size at runtime too. But i wanted to do something with templates.
template <std::size_t N>
using GameBoard = std::array<std::array<char, N>, N>;

// // Define aliases for the data structures
// // used for Move and TicTacToeBoard as they are common
// // and long
using TicTacToeBoard = GameBoard<3>;

// Declaration of the concept "Hashable", which is satisfied by any type 'T'
// such that for values 'a' of type 'T', the expression std::hash<T>{}(a)
// compiles and its result is convertible to std::size_t
// From: https://en.cppreference.com/w/cpp/language/constraints
template <typename T>
concept Hashable = requires(T a) {
                     { std::hash<T>{}(a) } -> std::convertible_to<std::size_t>;
                   };  // NOLINT [readability/braces]

// Specialization for unordered_set
template <Hashable T>
struct std::hash<std::tuple<T, T>> {
  std::size_t operator()(const std::tuple<T, T> &tuple) const noexcept {
    std::size_t h1 = std::hash<T>{}(std::get<0>(tuple));
    std::size_t h2 = std::hash<T>{}(std::get<1>(tuple));
    return h1 ^ (h2 << 1);  // or use boost::hash_combine
  }
};

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
  bool isAI{};     // cppcheck-suppress unusedStructMember
  int strength{};  // cppcheck-suppress unusedStructMember
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
    std::ranges::fill(row, '-');
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
  // Checking rows
  for (size_t i = 0; i < N; ++i) {
    if (std::all_of(board[i].begin(), board[i].end(),
                    [player](char cell) { return cell == player; })) {
      return true;
    }
  }

  // Checking columns
  for (size_t col = 0; col < N; ++col) {
    if (std::all_of(board.begin(), board.end(), [col, player](const auto &row) {
          return row[col] == player;
        })) {
      return true;
    }
  }

  // Checking diagonal
  bool diagonalWin{true};
  for (size_t i{0}; i < N; ++i) {
    if (board[i][i] != player) {
      diagonalWin = false;
      break;
    }
  }
  if (diagonalWin) {
    return true;
  }

  // Checking antidiagonal
  bool antidiagonalWin{true};
  for (size_t i{0}; i < N; ++i) {
    if (board[i][N - i - 1] != player) {
      antidiagonalWin = false;
      break;
    }
  }
  return antidiagonalWin;
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
    // Yes, with these sizes the ordered one is probably faster
    // But i wanted to do something more with templates.
    std::unordered_map<std::string,
                       std::unordered_set<std::tuple<std::size_t, std::size_t>>>
        *win_conditions) {
  for (size_t row{0}; row < N; row++) {
    for (size_t col{0}; col < N; col++) {
      auto &row_set = (*win_conditions)["row" + std::to_string(row)];
      auto &col_set = (*win_conditions)["col" + std::to_string(col)];
      auto &diag_set = (*win_conditions)["diag"];
      auto &antidiag_set = (*win_conditions)["antidiag"];

      // If the given player occupies this cell
      // then that reduces the required positions
      // in that line by one
      if (board[row][col] == player) {
        row_set.erase(std::make_tuple(row, col));
        col_set.erase(std::make_tuple(row, col));

        if (row == col) {
          diag_set.erase(std::make_tuple(row, col));
        }

        if (row == (N - 1 - col)) {
          antidiag_set.erase(std::make_tuple(row, col));
        }
      } else if (board[row][col] == swapPlayer(player)) {
        // If the opposing player occupies this cell
        // then all lines that contain it become useless
        row_set.clear();
        col_set.clear();

        if (row == col) {
          diag_set.clear();
        }

        if (row == (N - 1 - col)) {
          antidiag_set.clear();
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
                     std::unordered_set<std::tuple<std::size_t, std::size_t>>>
      win_conditions{};
  for (size_t row{0}; row < N; row++) {
    for (size_t col{0}; col < N; col++) {
      win_conditions["row" + std::to_string(row)].insert({row, col});
      win_conditions["col" + std::to_string(col)].insert({row, col});
      if (row == col) {
        win_conditions["diag"].insert({row, col});
      }
      if (row == (N - 1 - col)) {
        win_conditions["antidiag"].insert({row, col});
      }
    }
  }
  // Adjust wincondition requirements according to board state.
  checkWinconditions(player, board, &win_conditions);
  // Check if any line requires exactly one more
  // position from the player to be fulfilled
  auto it = std::find_if(win_conditions.begin(), win_conditions.end(),
                         [](const auto &x) { return x.second.size() == 1; });

  if (it != win_conditions.end()) {
    std::tuple<std::size_t, std::size_t> firstWin{*it->second.begin()};
    return {.spot = {.row{std::get<0>(firstWin)}, .col{std::get<1>(firstWin)}},
            .state{GameState::win}};
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
  Move winningMove{getWinningMove(player, board)};
  if (winningMove.state != GameState::undecided) {
    return winningMove;
  }
  return randomMove(board);
}

// Try to find a winning or blocking move
// If neither exists do a random one instead
template <std::size_t N>
Move blockWinMove(char player, const GameBoard<N> &board) {
  Move winningMove{getWinningMove(player, board)};
  if (winningMove.state != GameState::undecided) {
    return winningMove;
  }
  Move blockingMove{getBlockingMove(player, board)};
  if (blockingMove.state != GameState::undecided) {
    return blockingMove;
  }
  return randomMove(board);
}

// The coordinates of the optimal moves for the player on the board
template <std::size_t N>
BestMoves getBestMoves(char player, GameBoard<N> *board,
                       GameState bestX = GameState::loss,
                       GameState bestO = GameState::loss,
                       std::size_t max_depth = 10) {
  BestMoves best_moves{.spots{}, .state = GameState::undecided};
  // Base cases
  // Player won
  if (isPlayerWin(player, *board)) {
    best_moves.state = GameState::win;
    return best_moves;
  }
  // Player lost
  if (isPlayerWin(swapPlayer(player), *board)) {
    best_moves.state = GameState::loss;
    return best_moves;
  }

  const auto empty_cells{getEmptyCells(*board)};
  if (empty_cells.empty()) {
    // Game is drawn
    best_moves.state = GameState::draw;
    return best_moves;
  }
  // To reduce required computation
  // and increase replayability
  // just do a random move as the first
  // Optimal play still forces a draw
  if (empty_cells.size() >= max_depth || empty_cells.size() == N * N) {
    auto heuristic_move{randomMove(*board)};
    best_moves.spots.push_back(heuristic_move.spot);
    return best_moves;
  }
  // Recursively apply minmax algorithm
  for (const auto &cell : empty_cells) {
    (*board)[cell.row][cell.col] = player;
    auto current_moves{getBestMoves(swapPlayer(player), board)};
    if (-current_moves.state > best_moves.state) {
      best_moves.state = -current_moves.state;
      best_moves.spots = {cell};
    } else if (-current_moves.state == best_moves.state) {
      best_moves.spots.push_back(cell);
    }
    (*board)[cell.row][cell.col] = '-';
    // alpha-beta pruning part
    if (player == 'X') {
      bestX = std::max(bestX, best_moves.state);
      if (best_moves.state < bestO) {
        break;
      }
    } else {
      bestO = std::max(bestO, best_moves.state);
      if (best_moves.state < bestX) {
        break;
      }
    }
  }
  return best_moves;
}

// The coordinates of the one optimal move for the player on the board
template <std::size_t N>
Move minmax(char player, GameBoard<N> *board) {
  BestMoves best_moves{
      getBestMoves(player, board, GameState::undecided, GameState::undecided)};
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
