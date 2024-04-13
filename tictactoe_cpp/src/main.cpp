// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include <tictactoe.hpp>

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
      std::cout << "Player " << player << " wins the game!" << std::endl;
      break;
    }

    if (isBoardFilled(board)) {
      std::cout << "Match drawn!" << std::endl;
      break;
    }

    player = swapPlayer(player);
  }

  showBoard(board);

  return 0;
}
