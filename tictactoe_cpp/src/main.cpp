// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#include <tictactoe.h>

auto main() -> int {
  GameBoard board{createBoard()};
  char player{'X'};
  bool singlePlayer{getPlayerNumber()};
  char AI_marker{};
  int AI_strength{};
  if (singlePlayer) {
    AI_marker = getAIStart();
    AI_strength = getAIStrength();
  }

  while (true) {
    if (singlePlayer && player == AI_marker) {
      aiTurn(player, &board, AI_strength);
    } else {
      playerTurn(player, &board);
    }

    if (isPlayerWin(player, board)) {
      std::cout << "Player " << player << " wins the game!" << std::endl;
      break;
    }

    if (isBoardFilled(board)) {
      std::cout << "Match Draw" << std::endl;
      break;
    }

    player = swapPlayer(player);
  }

  showBoard(board);

  return 0;
}
