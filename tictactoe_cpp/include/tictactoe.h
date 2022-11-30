#ifndef TICTACTOE_H
#define TICTACTOE_H
#include <iostream>
#include <array>
#include <algorithm>
#include <random>
#include <chrono>
#include <thread>
#include <vector>

typedef std::array<std::array<std::string, 3>, 3> GameBoard;

GameBoard createBoard()
{
    GameBoard board = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
    return board;
}

bool getPlayerNumber()
{
    std::string solo;
    while (solo != "Y" && solo != "N")
    {
        std::cout << "Play alone vs AI?[y/n]" << std::endl;
        std::cin >> solo;
        std::transform(solo.begin(), solo.end(), solo.begin(), ::toupper);
    }
    if (solo == "Y")
    {
        return true;
    }
    return false;
}

bool isPlayerWin(const std::string &player, const GameBoard &board)
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

std::array<int, 3> minmax(const std::string player, GameBoard &board)
{
    std::array<int, 3> bestMove = {{-1, -1, -1}};
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
    for (const std::array<int, 2> &cell : emptyCells)
    {
        board[cell[0]][cell[1]] = player;
        std::array<int, 3> currentMove = minmax(swapPlayer(player), board);
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
    const int n = board.size();
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            std::cout << board[i][j] << " ";
        }
        std::cout << std::endl;
    }
}

void aiTurn(const std::string player, GameBoard &board)
{
    std::cout << "AI turn as player " << player << "." << std::endl;
    showBoard(board);
    std::array<int, 3> bestMove = minmax(player, board);
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