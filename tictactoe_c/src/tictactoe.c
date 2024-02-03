// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include "tictactoe.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

#define FLUSH_STDOUT_OR_RETURN_FALSE() \
  do {                                 \
    if (fflush(stdout) != 0) {         \
      perror("Error flushing stdout"); \
      return false;                    \
    }                                  \
  } while (0)

enum { BOARD_SIZE = 9 };
enum { WIN_PATTERNS_COUNT = 8 };
enum { PATTERN_SIZE = 3 };
const size_t WIN_PATTERNS[WIN_PATTERNS_COUNT][PATTERN_SIZE] = {
    {0, 1, 2}, {3, 4, 5}, {6, 7, 8},  // horizontal
    {0, 3, 6}, {1, 4, 7}, {2, 5, 8},  // vertical
    {0, 4, 8}, {2, 4, 6}              // diagonal
};

struct ConditionResult {
  size_t done;
  size_t open;
  int open_spots[PATTERN_SIZE];
};
typedef struct ConditionResult ConditionResult;

// Sadly 1 = '1' doesnt work.
/**
 * @brief Enumeration representing different values on the game board.
 */
enum GameValue {
  X = 'X',
  O = 'O',
  ONE = '1',
  TWO = '2',
  THREE = '3',
  FOUR = '4',
  FIVE = '5',
  SIX = '6',
  SEVEN = '7',
  EIGHT = '8',
  ZERO = '0'
};
typedef enum GameValue GameValue;

/**
 * @brief Enumeration representing different players in the game. Subset of
 * GameValue
 */
enum PlayerValue { PLAYER_X = X, PLAYER_O = O };
typedef enum PlayerValue PlayerValue;

/**
 * @brief Checks if a board spot is occupied.
 *
 * @param spot_value The value of the board spot to check.
 * @return true if the spot is occupied, false otherwise.
 */
static bool is_occupied(const GameValue spot_value) {
  return spot_value == X || spot_value == O;
}

/**
 * @brief Checks if a board spot is unoccupied.
 *
 * @param spot_value The value of the board spot to check.
 * @return true if the spot is unoccupied, false otherwise.
 */
static bool is_unoccupied(const GameValue spot_value) {
  return !is_occupied(spot_value);
}

/**
 * @brief Displays the current state of the game board.
 *
 * @param board The game board to be displayed.
 */
static void show_board(const GameValue board[static BOARD_SIZE]) {
  printf(" %c | %c | %c \n", board[0], board[1], board[2]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[3], board[4], board[5]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[6], board[7], board[8]);
}

/**
 * @brief Swaps between 'X' and 'O' players.
 *
 * @param player The current player.
 * @return The swapped player.
 */
PlayerValue swap_player(PlayerValue player) {
  if (player == PLAYER_X) {
    return PLAYER_O;
  }
  return PLAYER_X;
}

/**
 * @brief Makes a predictable move for the AI player on the game board.
 *
 * This function makes a move for the AI player by selecting the first
 * unoccupied spot on the board.
 *
 * @param player The player value representing the AI player.
 * @param board The game board.
 *
 */
static void predictable_move(const PlayerValue player,
                             GameValue board[static BOARD_SIZE]) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    if (is_unoccupied(board[i])) {
      board[i] = (GameValue)player;
      break;
    }
  }
}

static void check_pattern(const GameValue board[static BOARD_SIZE],
                          const size_t pattern[3], const PlayerValue player,
                          ConditionResult* result) {
  for (size_t i = 0; i < PATTERN_SIZE; i++) {
    if (board[pattern[i]] == (GameValue)player) {
      result->done++;
    } else if (is_unoccupied(board[pattern[i]])) {
      result->open_spots[result->open] = (int)pattern[i];
      result->open++;
    }
  }
}

static int get_blocking_spot(const PlayerValue player,
                             GameValue board[static BOARD_SIZE]) {
  for (int i = 0; i < WIN_PATTERNS_COUNT; i++) {
    ConditionResult result = {.done = 0, .open = 0, .open_spots = {-1, -1, -1}};
    check_pattern(board, WIN_PATTERNS[i], player, &result);
    if (result.done == 2 && result.open == 1) {
      return result.open_spots[0];
    }
  }
  return -1;
}

static void blocking_move(const PlayerValue player,
                          GameValue board[static BOARD_SIZE]) {
  const int blocking_spot = get_blocking_spot(swap_player(player), board);
  if (blocking_spot != -1) {
    board[blocking_spot] = (GameValue)player;
    return;
  }
  predictable_move(player, board);
}

/**
 * @brief AI's turn to make a move on the game board.
 *
 * @param player The player value representing the AI player.
 * @param board The game board.
 * @param strength The AI's strength level.
 * @return true if the move is successful, false otherwise.
 *
 * @note This function currently uses a simple strategy.
 * It also sleeps for 1 second to not be so jarring.
 * TODO: Add different AI strategies.
 */
static bool ai_turn(const PlayerValue player,
                    GameValue board[static BOARD_SIZE], const int strength) {
  printf("AI turn as player %c with strength %d.\n", player, strength);
  show_board(board);
  FLUSH_STDOUT_OR_RETURN_FALSE();
  switch (strength) {
    case 0:
      predictable_move(player, board);
      break;

    default:
      blocking_move(player, board);
      break;
  }
  sleep(1);
  return true;
}

/**
 * @brief Player's turn to make a move on the game board.
 *
 * Prompts the user for input. Checks that the input is an int in the
 * valid range and that the position is unoccupied.
 *
 * @param player The player value representing the human player.
 * @param board The game board.
 * @return true if the move is successful, false otherwise.
 */
static bool player_turn(const PlayerValue player,
                        GameValue board[static BOARD_SIZE]) {
  size_t position = 0;
  while (true) {
    printf("Player %c turn:\n", player);
    show_board(board);
    printf("Where to make your next move? [0-8]\n");
    FLUSH_STDOUT_OR_RETURN_FALSE();

    int chr = fgetc(stdin);
    int discard = 0;
    while (((discard = fgetc(stdin)) != '\n') && (discard != EOF)) {
      ;
    }

    if (chr >= '0' && (chr <= '9')) {
      position = (size_t)(chr - '0');
    } else {
      printf("Unexpected character: %c\n", chr);
      continue;
    }
    if (position >= BOARD_SIZE) {
      printf("Invalid position. Position must be less than %d.\n", BOARD_SIZE);
      continue;
    }

    if (is_occupied(board[position])) {
      printf("Position already taken. Choose another position.\n");
      continue;
    }

    break;  // Break out of the loop if all conditions are satisfied
  }
  board[position] = (GameValue)player;
  return true;
}

/**
 * @brief Checks if a player has won the game.
 *
 * @param player The player value to check for a win.
 * @param board The game board.
 * @return true if the player has won, false otherwise.
 */
static bool is_player_win(const PlayerValue player,
                          const GameValue board[static BOARD_SIZE]) {
  for (int i = 0; i < WIN_PATTERNS_COUNT; i++) {
    const size_t* pattern = WIN_PATTERNS[i];
    if (board[pattern[0]] == (GameValue)player &&
        board[pattern[1]] == (GameValue)player &&
        board[pattern[2]] == (GameValue)player) {
      return true;
    }
  }
  return false;
}

/**
 * @brief Checks if the game board is completely filled.
 *
 * If used after is_player_win, this function can be used to check for a draw.
 *
 * @param board The game board.
 * @return true if the board is filled, false otherwise.
 */
static bool is_board_filled(const GameValue board[static BOARD_SIZE]) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    if (is_unoccupied(board[i])) {
      return false;
    }
  }
  return true;
}

/**
 * @brief Main function to play the Tic Tac Toe game.
 *
 * @param playerX_strength The strength level of Player X (negative for human
 * player).
 * @param playerO_strength The strength level of Player O (negative for human
 * player).
 * @return true if the game is played successfully, false otherwise.
 */
bool play_game(int playerX_strength, int playerO_strength) {
  GameValue board[BOARD_SIZE] = {ZERO, ONE, TWO,   THREE, FOUR,
                                 FIVE, SIX, SEVEN, EIGHT};
  PlayerValue player = PLAYER_X;
  while (true) {
    if (player == PLAYER_X && playerX_strength >= 0) {
      if (!ai_turn(player, board, playerX_strength)) {
        return false;
      }
    } else if (player == PLAYER_O && playerO_strength >= 0) {
      if (!ai_turn(player, board, playerO_strength)) {
        return false;
      }
    } else {
      if (!player_turn(player, board)) {
        return false;
      }
    }

    if (is_player_win(player, board)) {
      printf("Player %c wins the game!\n", player);
      break;
    }

    if (is_board_filled(board)) {
      printf("Match Draw\n");
      break;
    }

    player = swap_player(player);
  }
  show_board(board);
  FLUSH_STDOUT_OR_RETURN_FALSE();
  return true;
}
