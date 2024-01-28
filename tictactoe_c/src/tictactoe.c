// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include "tictactoe.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void player_turn(const char player, char board[static BOARD_SIZE]) {
  size_t position = 0;
  while (true) {
    printf("Player %c turn:\n", player);
    show_board(board);
    printf("Where to make your next move? [0-8]\n");
    if (fflush(stdout) != 0) {
      perror("Error flushing stdout");
      exit(EXIT_FAILURE);
      // Handle the error or exit the program
    }
    // Read the next character in the input buffer
    int chr = fgetc(stdin);
    int discard = 0;
    while (((discard = fgetc(stdin)) != '\n') && (discard != EOF)) {
      ;
    }
    // Check if the value is in range
    if (chr >= '0' && (chr <= '9')) {
      position = (size_t)(chr - '0');
    } else {
      printf("Unexpected character: %c\n", chr);
      continue;
    }
    // Remove remaining characters from the
    // input buffer.

    if (position >= BOARD_SIZE) {
      printf("Invalid position. Position must be less than %d.\n", BOARD_SIZE);
      continue;
    }

    if (board[position] == 'X' || board[position] == 'O') {
      printf("Position already taken. Choose another position.\n");
      continue;
    }

    break;  // Break out of the loop if all conditions are satisfied
  }
  board[position] = player;
}

bool is_player_win(const char player, const char board[static BOARD_SIZE]) {
  const size_t win_patterns[8][3] = {
      {0, 1, 2}, {3, 4, 5}, {6, 7, 8},  // horizontal
      {0, 3, 6}, {1, 4, 7}, {2, 5, 8},  // vertical
      {0, 4, 8}, {2, 4, 6}              // diagonal
  };
  for (int i = 0; i < 8; i++) {
    const size_t* pattern = win_patterns[i];
    if (board[pattern[0]] == player && board[pattern[1]] == player &&
        board[pattern[2]] == player) {
      return true;
    }
  }
  return false;
}
bool is_board_filled(const char board[static BOARD_SIZE]) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    if (board[i] != 'X' && board[i] != 'O') {
      return false;
    }
  }
  return true;
}
char swap_player(char player) {
  if (player == 'X') {
    return 'O';
  }
  return 'X';
}

void show_board(const char board[static BOARD_SIZE]) {
  printf(" %c | %c | %c \n", board[0], board[1], board[2]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[3], board[4], board[5]);
  printf("---+---+---\n");
  printf(" %c | %c | %c \n", board[6], board[7], board[8]);
}

void play_game(void) {
  char board[BOARD_SIZE] = {'0', '1', '2', '3', '4', '5', '6', '7', '8'};
  char player = 'X';
  while (true) {
    player_turn(player, board);

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
}
