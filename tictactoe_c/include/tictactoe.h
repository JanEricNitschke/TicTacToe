// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#ifndef TICTACTOE_C_INCLUDE_TICTACTOE_H_
#define TICTACTOE_C_INCLUDE_TICTACTOE_H_

#include <stdbool.h>
enum { BOARD_SIZE = 9 };
void play_game(void);
void player_turn(char player, char board[static BOARD_SIZE]);
bool is_player_win(char player, const char board[static BOARD_SIZE]);
bool is_board_filled(const char board[static BOARD_SIZE]);
char swap_player(char player);
void show_board(const char board[static BOARD_SIZE]);

#endif  // TICTACTOE_C_INCLUDE_TICTACTOE_H_
