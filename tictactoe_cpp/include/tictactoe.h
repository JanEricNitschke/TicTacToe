#ifndef TICTACTOE_H
#define TICTACTOE_H
#include <iostream>
#include <array>
#include <algorithm>
#include <random>
#include <chrono>
#include <thread>
#include <vector>
#include <map>
#include <set>
#include <tuple>

typedef std::array<std::array<std::string, 3>, 3>
    GameBoard;
typedef std::array<int, 3> Move;

GameBoard createBoard()
{
    GameBoard board = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
    return board;
}

bool getPlayerYesNo(const std::string question)
{
    std::string solo;
    while (solo != "Y" && solo != "N")
    {
        std::cout << question << std::endl;
        std::cin >> solo;
        std::transform(solo.begin(), solo.end(), solo.begin(), ::toupper);
    }
    if (solo == "Y")
    {
        return true;
    }
    return false;
}

bool getPlayerNumber()
{
    return getPlayerYesNo("Play alone vs AI?[y/n]: ");
}

std::string getAIStart()
{
    if (getPlayerYesNo("Should the AI make the first move?[y/n]: "))
    {
        return "X";
    }
    return "O";
}

int getAIStrength()
{
    std::string input;
    int strength = -1;
    std::cout << "AI strength settings:" << std::endl;
    std::cout << "1: Easy" << std::endl;
    std::cout << "2: Medium" << std::endl;
    std::cout << "3: Hard" << std::endl;
    std::cout << "4: Impossible" << std::endl;
    while (true)
    {
        std::cout << "How strong should the AI be?[1-4]: " << std::endl;
        std::cin >> input;
        try
        {
            strength = std::stoi(input);
        }
        catch (std::invalid_argument const &ex)
        {
            std::cout << "Invalid input" << std::endl;
            continue;
        }
        if (strength > 4 || strength < 1)
        {
            std::cout << "Invalid input" << std::endl;
            continue;
        }
        break;
    }
    return strength;
}

bool isPlayerWin(const std::string player, const GameBoard &board)
{
    bool win = false;
    const int n = board.size();

    // checking rows
    for (int i = 0; i < n; i++)
    {
        win = true;
        for (int j = 0; j < n; j++)
        {
            if (board[i][j] != player)
            {
                win = false;
                break;
            }
        }
        if (win)
        {
            return win;
        }
    }

    // checking cols
    for (int i = 0; i < n; i++)
    {
        win = true;
        for (int j = 0; j < n; j++)
        {
            if (board[j][i] != player)
            {
                win = false;
                break;
            }
        }
        if (win)
        {
            return win;
        }
    }

    // checking diagonal
    win = true;
    for (int i = 0; i < n; i++)
    {
        if (board[i][i] != player)
        {
            win = false;
            break;
        }
    }
    if (win)
    {
        return win;
    }

    win = true;
    for (int i = 0; i < n; i++)
    {
        if (board[i][n - i - 1] != player)
        {
            win = false;
            break;
        }
    }
    if (win)
    {
        return win;
    }

    return false;
}

bool isBoardFilled(const GameBoard &board)
{
    const int n = board.size();
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (board[j][i] == "-")
            {
                return false;
            }
        }
    }
    return true;
}

std::string swapPlayer(const std::string player)
{
    return (player == "X") ? "O" : "X";
}

std::vector<std::array<int, 2>> getEmptyCells(const GameBoard &board)
{
    const int n = board.size();
    std::vector<std::array<int, 2>> emptyCells;
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (board[i][j] == "-")
            {
                emptyCells.push_back({{i, j}});
            }
        }
    }
    return emptyCells;
}

Move randomMove(const std::string player, GameBoard &board)
{
    const std::vector<std::array<int, 2>> emptyCells = getEmptyCells(board);
    std::array<int, 2> chosenCell = emptyCells[std::rand() % emptyCells.size()];
    return {{chosenCell[0], chosenCell[1], 0}};
}

Move getWinningMove(const std::string player, GameBoard &board)
{
    std::map<std::string, std::set<std::tuple<int, int>>> win_conditions{
        {"row0", std::set<std::tuple<int, int>>({std::make_tuple(0, 0), std::make_tuple(0, 1), std::make_tuple(0, 2)})},
        {"row1", std::set<std::tuple<int, int>>({std::make_tuple(1, 0), std::make_tuple(1, 1), std::make_tuple(1, 2)})},
        {"row2", std::set<std::tuple<int, int>>({std::make_tuple(2, 0), std::make_tuple(2, 1), std::make_tuple(2, 2)})},
        {"col0", std::set<std::tuple<int, int>>({std::make_tuple(0, 0), std::make_tuple(1, 0), std::make_tuple(2, 0)})},
        {"col1", std::set<std::tuple<int, int>>({std::make_tuple(0, 1), std::make_tuple(1, 1), std::make_tuple(2, 1)})},
        {"col2", std::set<std::tuple<int, int>>({std::make_tuple(0, 2), std::make_tuple(1, 2), std::make_tuple(2, 2)})},
        {"diag", std::set<std::tuple<int, int>>({std::make_tuple(0, 0), std::make_tuple(1, 1), std::make_tuple(2, 2)})},
        {"antidiag", std::set<std::tuple<int, int>>({std::make_tuple(0, 2), std::make_tuple(1, 1), std::make_tuple(2, 0)})}};
    const int n = board.size();
    for (int row = 0; row < n; row++)
    {
        for (int col = 0; col < n; col++)
        {
            if (board[row][col] == player)
            {
                if (win_conditions["row" + std::to_string(row)].find(std::make_tuple(row, col)) != win_conditions["row" + std::to_string(row)].end())
                {
                    win_conditions["row" + std::to_string(row)].erase(std::make_tuple(row, col));
                }
                if (win_conditions["col" + std::to_string(col)].find(std::make_tuple(row, col)) != win_conditions["col" + std::to_string(col)].end())
                {
                    win_conditions["col" + std::to_string(col)].erase(std::make_tuple(row, col));
                }
                if (row == col && win_conditions["diag"].find(std::make_tuple(row, col)) != win_conditions["diag"].end())
                {
                    win_conditions["diag"].erase(std::make_tuple(row, col));
                }
                if (row == (n - 1 - col) && win_conditions["antidiag"].find(std::make_tuple(row, col)) != win_conditions["antidiag"].end())
                {
                    win_conditions["antidiag"].erase(std::make_tuple(row, col));
                }
            }
            if (board[row][col] == swapPlayer(player))
            {
                win_conditions["row" + std::to_string(row)].clear();
                win_conditions["col" + std::to_string(col)].clear();
                if (row == col)
                {
                    win_conditions["diag"].clear();
                }
                if (row == (n - 1 - col))
                {
                    win_conditions["antidiag"].clear();
                }
            }
        }
    }
    for (auto const &x : win_conditions)
    {
        if (x.second.size() == 1)
        {
            std::tuple<int, int> firstWin = *x.second.begin();
            Move winMove = {{std::get<0>(firstWin), std::get<1>(firstWin), 0}};
            return winMove;
        }
    }
    return {{-1, -1, -1}};
}

Move getBlockingMove(const std::string player, GameBoard &board)
{
    return getWinningMove(swapPlayer(player), board);
}

Move winMove(const std::string player, GameBoard &board)
{
    Move winMove = getWinningMove(player, board);
    if (winMove[2] == 0)
    {
        return winMove;
    }
    return randomMove(player, board);
}

Move blockWinMove(const std::string player, GameBoard &board)
{
    Move winMove = getWinningMove(player, board);
    if (winMove[2] == 0)
    {
        return winMove;
    }
    Move blockMove = getBlockingMove(player, board);
    if (blockMove[2] == 0)
    {
        return blockMove;
    }
    return randomMove(player, board);
}

Move minmax(const std::string player, GameBoard &board)
{
    Move bestMove = {{-1, -1, -1}};
    if (isPlayerWin(player, board))
    {
        bestMove[2] = 1;
        return bestMove;
    }
    if (isPlayerWin(swapPlayer(player), board))
    {
        bestMove[2] = -1;
        return bestMove;
    }
    const std::vector<std::array<int, 2>> emptyCells = getEmptyCells(board);
    if (emptyCells.size() == 0)
    {
        bestMove[2] = 0;
        return bestMove;
    }
    if (emptyCells.size() == 9)
    {
        bestMove[0] = std::rand() % 3;
        bestMove[1] = std::rand() % 3;
        bestMove[2] = 0;
        return bestMove;
    }
    for (const std::array<int, 2> &cell : emptyCells)
    {
        board[cell[0]][cell[1]] = player;
        Move currentMove = minmax(swapPlayer(player), board);
        if (-currentMove[2] > bestMove[2])
        {
            bestMove = {{cell[0], cell[1], -currentMove[2]}};
        }
        board[cell[0]][cell[1]] = "-";
    }
    return bestMove;
}

void showBoard(const GameBoard &board)
{
    std::string line_separator = "---------------";
    const int n = board.size();
    std::cout << line_separator << std::endl;
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            std::cout << "| " << board[i][j] << " |";
        }
        std::cout << std::endl
                  << line_separator << std::endl;
    }
}

void aiTurn(const std::string player, GameBoard &board, int AI_strength)
{
    std::cout << "AI turn as player " << player << "." << std::endl;
    showBoard(board);
    Move bestMove;
    switch (AI_strength)
    {
    case 1:
        bestMove = randomMove(player, board);
        break;
    case 2:
        bestMove = winMove(player, board);
        break;
    case 3:
        bestMove = blockWinMove(player, board);
        break;
    default:
        bestMove = minmax(player, board);
    }
    board[bestMove[0]][bestMove[1]] = player;
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}

void playerTurn(const std::string player, GameBoard &board)
{
    int row = -1;
    int col = -1;
    bool validMove = false;
    while (!validMove)
    {
        std::cout << "Player " << player << " turn" << std::endl;
        showBoard(board);
        validMove = true;
        std::string input1;
        std::string input2;
        std::cout << "Enter row and column numbers to fix spot: " << std::endl;
        std::cin >> input1 >> input2;
        try
        {
            row = std::stoi(input1);
            col = std::stoi(input2);
        }
        catch (std::invalid_argument const &ex)
        {
            std::cout << "At least one of your entered inputs of (" << input1 << ", " << input2 << ") could not be converted to an integer. Try again!" << std::endl;
            validMove = false;
            continue;
        }
        if (row > 3 || row < 1 || col > 3 || col < 1)
        {
            std::cout << "Row " << row << " or column " << col << " are out of bounds. They have to be between 1 and 3 inclusive. Try again!" << std::endl;
            validMove = false;
            continue;
        }
        if (board[row - 1][col - 1] != "-")
        {
            std::cout << "The position (" << row << ", " << col << ") has already been taken by a player! Please do your move on an empty position." << std::endl;
            validMove = false;
            continue;
        }
    }
    board[row - 1][col - 1] = player;
}
#endif