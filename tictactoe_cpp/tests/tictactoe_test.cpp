// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <tictactoe.h>

// Demonstrate some basic assertions.
TEST(createBoardTest, createBoard) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  GameBoard producedBoard = createBoard();
  EXPECT_EQ(refBoard, producedBoard);
}

TEST(isPlayerWin, emptyBoard) {
  // empty board
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin, rows) {
  // check rows
  GameBoard refBoard = {{{"X", "X", "X"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_TRUE(isPlayerWin("X", refBoard));
  EXPECT_FALSE(isPlayerWin("O", refBoard));
  refBoard = {{{"X", "X", "-"}, {"-", "-", "X"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin, cols) {
  // check cols
  GameBoard refBoard = {{{"X", "O", "X"}, {"X", "-", "-"}, {"X", "-", "-"}}};
  EXPECT_TRUE(isPlayerWin("X", refBoard));
  EXPECT_FALSE(isPlayerWin("O", refBoard));
  refBoard = {{{"X", "O", "X"}, {"X", "-", "-"}, {"O", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin, diagonals) {
  // check diagonal
  GameBoard refBoard = {{{"O", "O", "X"}, {"X", "O", "-"}, {"X", "-", "O"}}};
  EXPECT_TRUE(isPlayerWin("O", refBoard));
  EXPECT_FALSE(isPlayerWin("X", refBoard));
  refBoard = {{{"O", "O", "X"}, {"X", "O", "-"}, {"X", "-", "X"}}};
  EXPECT_FALSE(isPlayerWin("O", refBoard));

  // check reverse diagonal
  refBoard = {{{"X", "O", "O"}, {"-", "O", "X"}, {"O", "-", "X"}}};
  EXPECT_TRUE(isPlayerWin("O", refBoard));
  EXPECT_FALSE(isPlayerWin("X", refBoard));
  refBoard = {{{"X", "O", "O"}, {"-", "O", "X"}, {"X", "-", "X"}}};
  EXPECT_FALSE(isPlayerWin("O", refBoard));
}

TEST(isBoardFilled, emptyBoard) {
  // empty board
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
}

TEST(isBoardFilled, filledBoards) {
  // empty board
  GameBoard refBoard = {{{"X", "X", "X"}, {"X", "X", "X"}, {"X", "X", "X"}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{"O", "O", "O"}, {"O", "O", "O"}, {"O", "O", "O"}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{"O", "X", "O"}, {"X", "O", "O"}, {"O", "O", "X"}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{"X", "O", "O"}, {"X", "O", "O"}, {"X", "O", "O"}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
}

TEST(isBoardFilled, nonFilledBoards) {
  // empty board
  GameBoard refBoard = {{{"-", "X", "X"}, {"X", "X", "X"}, {"X", "X", "X"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{"-", "O", "O"}, {"O", "-", "O"}, {"O", "O", "O"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{"-", "X", "O"}, {"-", "O", "O"}, {"-", "O", "X"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{"-", "O", "O"}, {"X", "-", "O"}, {"X", "O", "-"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
}

TEST(swapPlayer, swappingPlayers) {
  EXPECT_EQ("X", swapPlayer("O"));
  EXPECT_EQ("O", swapPlayer("X"));
}

TEST(getEmptyCells, emptyBoard) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  std::vector<std::array<int, 2>> expectedCells = {
      {0, 0}, {0, 1}, {0, 2}, {2, 0}, {2, 1}, {2, 2}, {1, 0}, {1, 1}, {1, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells, someEmptyCells) {
  GameBoard refBoard = {{{"X", "-", "X"}, {"O", "O", "O"}, {"-", "O", "-"}}};
  std::vector<std::array<int, 2>> expectedCells = {{0, 1}, {2, 0}, {2, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
  refBoard = {{{"-", "X", "X"}, {"O", "O", "O"}, {"X", "O", "O"}}};
  expectedCells = {{0, 0}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells, fullBoard) {
  GameBoard refBoard = {{{"X", "O", "X"}, {"O", "X", "O"}, {"X", "O", "X"}}};
  std::vector<std::array<int, 2>> expectedCells = {};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(minmax, emptyBoard) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  std::vector<std::array<int, 3>> possibleResults = {{
      {0, 0, 0},
      {0, 1, 0},
      {0, 2, 0},
      {1, 0, 0},
      {1, 1, 0},
      {1, 2, 0},
      {2, 0, 0},
      {2, 1, 0},
      {2, 2, 0},
  }};
  EXPECT_THAT(possibleResults, ::testing::Contains(minmax("X", &refBoard)));
}

TEST(minmax, fullBoard) {
  GameBoard refBoard = {{{"X", "X", "X"}, {"X", "X", "X"}, {"X", "X", "X"}}};
  std::array<int, 3> expectedResult = {
      {-1, -1, 1},
  };
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  expectedResult = {
      {-1, -1, -1},
  };
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
}

TEST(minmax, partialBoards) {
  GameBoard refBoard = {{{"X", "X", "-"}, {"O", "X", "O"}, {"X", "O", "O"}}};
  std::array<int, 3> expectedResult = {
      {0, 2, 1},
  };
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
  refBoard = {{{"O", "O", "X"}, {"X", "-", "O"}, {"-", "O", "X"}}};
  expectedResult = {
      {1, 1, 0},
  };
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  refBoard = {{{"O", "O", "X"}, {"X", "-", "-"}, {"-", "O", "X"}}};
  expectedResult = {
      {1, 1, 1},
  };
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
  refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  expectedResult = {
      {1, 1, 0},
  };
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
}

TEST(getWinningMove, row) {
  GameBoard refBoard = {{{"O", "X", "O"}, {"X", "O", "-"}, {"X", "X", "-"}}};
  Move expectedResult = {{2, 2, 0}};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getWinningMove, col) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {{1, 0, 0}};
  EXPECT_THAT(expectedResult, getWinningMove("O", refBoard));
}

TEST(getWinningMove, diagonal) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "O", "-"}, {"-", "-", "-"}}};
  Move expectedResult = {{2, 2, 0}};
  EXPECT_THAT(expectedResult, getWinningMove("O", refBoard));
}

TEST(getWinningMove, antidiagonal) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "X", "-"}, {"X", "-", "-"}}};
  Move expectedResult = {{0, 2, 0}};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getWinningMove, none) {
  GameBoard refBoard = {{{"O", "X", "X"}, {"-", "O", "-"}, {"O", "X", "-"}}};
  Move expectedResult = {{-1, -1, -1}};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getBlockingMove, row) {
  GameBoard refBoard = {{{"O", "X", "O"}, {"X", "O", "-"}, {"X", "X", "-"}}};
  Move expectedResult = {{2, 2, 0}};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(getBlockingMove, col) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {{1, 0, 0}};
  EXPECT_THAT(expectedResult, getBlockingMove("X", refBoard));
}

TEST(getBlockingMove, diagonal) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "O", "-"}, {"-", "-", "-"}}};
  Move expectedResult = {{2, 2, 0}};
  EXPECT_THAT(expectedResult, getBlockingMove("X", refBoard));
}

TEST(getBlockingMove, antidiagonal) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "X", "-"}, {"X", "-", "-"}}};
  Move expectedResult = {{0, 2, 0}};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(getBlockingMove, none) {
  GameBoard refBoard = {{{"O", "X", "X"}, {"-", "O", "-"}, {"O", "X", "-"}}};
  Move expectedResult = {{-1, -1, -1}};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(randomMove, emptyBoard) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = randomMove(refBoard);
  EXPECT_TRUE(result[0] >= 0 && result[0] < 3);
  EXPECT_TRUE(result[1] >= 0 && result[1] < 3);
  EXPECT_EQ(result[2], 0);
}

TEST(randomMove, oneOption) {
  GameBoard refBoard = {{{"X", "X", "-"}, {"O", "X", "O"}, {"X", "O", "O"}}};
  Move expectedResult = {{0, 2, 0}};
  EXPECT_THAT(expectedResult, randomMove(refBoard));
}

TEST(winMove, prioritizesWin) {
  GameBoard refBoard = {{{"O", "-", "X"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {{1, 0, 0}};
  EXPECT_THAT(expectedResult, winMove("O", refBoard));
}

TEST(winMove, worksWithoutWin) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = winMove("X", refBoard);
  EXPECT_TRUE(result[0] >= 0 && result[0] < 3);
  EXPECT_TRUE(result[1] >= 0 && result[1] < 3);
  EXPECT_EQ(result[2], 0);
}

TEST(blockWinMove, prioritizesWin) {
  GameBoard refBoard = {{{"X", "-", "O"}, {"-", "-", "-"}, {"X", "-", "O"}}};
  Move expectedResult = {{1, 0, 0}};
  EXPECT_THAT(expectedResult, blockWinMove("X", refBoard));
}

TEST(blockWinMove, blocksIfNoWin) {
  GameBoard refBoard = {{{"X", "-", "O"}, {"-", "-", "-"}, {"-", "-", "O"}}};
  Move expectedResult = {{1, 2, 0}};
  EXPECT_THAT(expectedResult, blockWinMove("X", refBoard));
}

TEST(blockWinMove, worksWithoutBlockWin) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = blockWinMove("X", refBoard);
  EXPECT_TRUE(result[0] >= 0 && result[0] < 3);
  EXPECT_TRUE(result[1] >= 0 && result[1] < 3);
  EXPECT_EQ(result[2], 0);
}
