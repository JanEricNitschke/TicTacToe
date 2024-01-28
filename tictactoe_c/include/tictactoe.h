// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#ifndef TICTACTOE_C_INCLUDE_TICTACTOE_H_
#define TICTACTOE_C_INCLUDE_TICTACTOE_H_

#include <stdbool.h>
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
void play_game(int playerX_strength, int playerO_strength);
void player_turn(PlayerValue player, GameValue board[static BOARD_SIZE]);
void ai_turn(PlayerValue player, GameValue board[static BOARD_SIZE],
             int strength);
bool is_player_win(PlayerValue player,
                   const GameValue board[static BOARD_SIZE]);
bool is_board_filled(const GameValue board[static BOARD_SIZE]);
PlayerValue swap_player(PlayerValue player);
void show_board(const GameValue board[static BOARD_SIZE]);
bool is_occupied(GameValue spot_value);
bool is_unoccupied(GameValue spot_value);
void flush_output(void);

#endif  // TICTACTOE_C_INCLUDE_TICTACTOE_H_
