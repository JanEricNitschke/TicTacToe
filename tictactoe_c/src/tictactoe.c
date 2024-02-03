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

// Sadly 1 = '1' doesnt work.
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

enum PlayerValue { PLAYER_X = X, PLAYER_O = O };
typedef enum PlayerValue PlayerValue;

static bool is_occupied(const GameValue spot_value) {
  return spot_value == X || spot_value == O;
}

static bool is_unoccupied(const GameValue spot_value) {
  return !is_occupied(spot_value);
}

static void show_board(const GameValue board[static BOARD_SIZE]) {
  printf(" %c | %c | %c \n", board[0], board[1], board[2]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[3], board[4], board[5]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[6], board[7], board[8]);
}

static bool ai_turn(const PlayerValue player,
                    GameValue board[static BOARD_SIZE], const int strength) {
  printf("AI turn as player %c with strength %d.\n", player, strength);
  show_board(board);
  FLUSH_STDOUT_OR_RETURN_FALSE();
  for (int i = 0; i < BOARD_SIZE; i++) {
    if (is_unoccupied(board[i])) {
      board[i] = (GameValue)player;
      break;
    }
  }
  sleep(1);
  return true;
}

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

static bool is_player_win(const PlayerValue player,
                          const GameValue board[static BOARD_SIZE]) {
  const size_t win_patterns[8][3] = {
      {0, 1, 2}, {3, 4, 5}, {6, 7, 8},  // horizontal
      {0, 3, 6}, {1, 4, 7}, {2, 5, 8},  // vertical
      {0, 4, 8}, {2, 4, 6}              // diagonal
  };
  for (int i = 0; i < 8; i++) {
    const size_t* pattern = win_patterns[i];
    if (board[pattern[0]] == (GameValue)player &&
        board[pattern[1]] == (GameValue)player &&
        board[pattern[2]] == (GameValue)player) {
      return true;
    }
  }
  return false;
}

static bool is_board_filled(const GameValue board[static BOARD_SIZE]) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    if (is_unoccupied(board[i])) {
      return false;
    }
  }
  return true;
}

PlayerValue swap_player(PlayerValue player) {
  if (player == PLAYER_X) {
    return PLAYER_O;
  }
  return PLAYER_X;
}

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
