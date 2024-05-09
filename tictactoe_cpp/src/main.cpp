// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include <iostream>
#include <tictactoe.hpp>

#include <print>

auto main() -> int {
  auto board{createBoard<4>()};
  char player{'X'};

  AISettings xSettings{getAISettings('X')};
  AISettings oSettings{getAISettings('O')};

  while (true) {
    if (player == 'X' && xSettings.isAI) {
      aiTurn(player, &board, xSettings.strength);
    } else if (player == 'O' && oSettings.isAI) {
      aiTurn(player, &board, oSettings.strength);
    } else {
      playerTurn(player, &board);
    }

    if (isPlayerWin(player, board)) {
      std::println("Player {} wins the game!", player);
      break;
    }

    if (isBoardFilled(board)) {
      std::println("Match drawn!");
      break;
    }

    player = swapPlayer(player);
  }

  showBoard(board);

  return 0;
}
