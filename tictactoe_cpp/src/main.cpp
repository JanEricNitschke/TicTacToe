// C++ program to display "Hello World"

// Header file for input output functions
#include <tictactoe.h>

// Main() function: where the execution of program begins

int main()
{
    srand(static_cast<unsigned int>(time(NULL)));
    GameBoard board = createBoard();
    std::string player = "X";
    bool singlePlayer = getPlayerNumber();
    std::string AI_marker;
    int AI_strength;
    if (singlePlayer)
    {
        AI_marker = getAIStart();
        AI_strength = getAIStrength();
    }

    while (true)
    {
        if (singlePlayer && player == AI_marker)
        {
            aiTurn(player, board, AI_strength);
        }
        else
        {
            playerTurn(player, board);
        }

        if (isPlayerWin(player, board))
        {
            std::cout << "Player " << player << " wins the game!" << std::endl;
            break;
        }

        if (isBoardFilled(board))
        {
            std::cout << "Match Draw" << std::endl;
            break;
        }

        player = swapPlayer(player);
    }

    showBoard(board);

    return 0;
}
