// C++ program to display "Hello World"

// Header file for input output functions
#include <tictactoe.h>

// Main() function: where the execution of program begins

int main()
{
    srand((unsigned int)time(NULL));
    GameBoard board = createBoard();
    std::string player = ((rand() % 2) == 0) ? "X" : "O";
    bool singlePlayer = getPlayerNumber();
    while (true)
    {
        if (singlePlayer && player == "X")
        {
            aiTurn(player, board);
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