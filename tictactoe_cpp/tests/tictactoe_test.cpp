// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <tictactoe.h>

// Demonstrate some basic assertions.
TEST(createBoardTest /*unused*/, createBoard /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  GameBoard producedBoard = createBoard();
  EXPECT_EQ(refBoard, producedBoard);
}

TEST(isPlayerWin /*unused*/, emptyBoard /*unused*/) {
  // empty board
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin /*unused*/, rows /*unused*/) {
  // check rows
  GameBoard refBoard = {{{"X", "X", "X"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_TRUE(isPlayerWin("X", refBoard));
  EXPECT_FALSE(isPlayerWin("O", refBoard));
  refBoard = {{{"X", "X", "-"}, {"-", "-", "X"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin /*unused*/, cols /*unused*/) {
  // check cols
  GameBoard refBoard = {{{"X", "O", "X"}, {"X", "-", "-"}, {"X", "-", "-"}}};
  EXPECT_TRUE(isPlayerWin("X", refBoard));
  EXPECT_FALSE(isPlayerWin("O", refBoard));
  refBoard = {{{"X", "O", "X"}, {"X", "-", "-"}, {"O", "-", "-"}}};
  EXPECT_FALSE(isPlayerWin("X", refBoard));
}

TEST(isPlayerWin /*unused*/, diagonals /*unused*/) {
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

TEST(isBoardFilled /*unused*/, emptyBoard /*unused*/) {
  // empty board
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
}

TEST(isBoardFilled /*unused*/, filledBoards /*unused*/) {
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

TEST(isBoardFilled /*unused*/, nonFilledBoards /*unused*/) {
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

TEST(swapPlayer /*unused*/, swappingPlayers /*unused*/) {
  EXPECT_EQ("X", swapPlayer("O"));
  EXPECT_EQ("O", swapPlayer("X"));
}

TEST(getEmptyCells /*unused*/, emptyBoard /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  std::vector<std::array<size_t, 2>> expectedCells = {
      {0, 0}, {0, 1}, {0, 2}, {2, 0}, {2, 1}, {2, 2}, {1, 0}, {1, 1}, {1, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells /*unused*/, someEmptyCells /*unused*/) {
  GameBoard refBoard = {{{"X", "-", "X"}, {"O", "O", "O"}, {"-", "O", "-"}}};
  std::vector<std::array<size_t, 2>> expectedCells = {{0, 1}, {2, 0}, {2, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
  refBoard = {{{"-", "X", "X"}, {"O", "O", "O"}, {"X", "O", "O"}}};
  expectedCells = {{0, 0}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells /*unused*/, fullBoard /*unused*/) {
  GameBoard refBoard = {{{"X", "O", "X"}, {"O", "X", "O"}, {"X", "O", "X"}}};
  std::vector<std::array<size_t, 2>> expectedCells = {};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(minmax /*unused*/, emptyBoard /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  std::vector<Move> possibleResults = {{
      Move{0, 0, 0},
      Move{0, 1, 0},
      Move{0, 2, 0},
      Move{1, 0, 0},
      Move{1, 1, 0},
      Move{1, 2, 0},
      Move{2, 0, 0},
      Move{2, 1, 0},
      Move{2, 2, 0},
  }};
  EXPECT_THAT(possibleResults, ::testing::Contains(minmax("X", &refBoard)));
}

TEST(minmax /*unused*/, fullBoard /*unused*/) {
  GameBoard refBoard = {{{"X", "X", "X"}, {"X", "X", "X"}, {"X", "X", "X"}}};
  Move expectedResult = {.row{0}, .col{0}, .state{1}};
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  expectedResult = {.row{0}, .col{0}, .state{-1}};
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
}

TEST(minmax /*unused*/, partialBoards /*unused*/) {
  GameBoard refBoard = {{{"X", "X", "-"}, {"O", "X", "O"}, {"X", "O", "O"}}};
  Move expectedResult = {0, 2, 1};
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
  refBoard = {{{"O", "O", "X"}, {"X", "-", "O"}, {"-", "O", "X"}}};
  expectedResult = {1, 1, 0};
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
  refBoard = {{{"O", "O", "X"}, {"X", "-", "-"}, {"-", "O", "X"}}};
  expectedResult = {1, 1, 1};
  EXPECT_THAT(expectedResult, minmax("O", &refBoard));
  refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  expectedResult = {1, 1, 0};
  EXPECT_THAT(expectedResult, minmax("X", &refBoard));
}

TEST(getWinningMove /*unused*/, row /*unused*/) {
  GameBoard refBoard = {{{"O", "X", "O"}, {"X", "O", "-"}, {"X", "X", "-"}}};
  Move expectedResult = {2, 2, 0};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getWinningMove /*unused*/, col /*unused*/) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {1, 0, 0};
  EXPECT_THAT(expectedResult, getWinningMove("O", refBoard));
}

TEST(getWinningMove /*unused*/, diagonal /*unused*/) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "O", "-"}, {"-", "-", "-"}}};
  Move expectedResult = {2, 2, 0};
  EXPECT_THAT(expectedResult, getWinningMove("O", refBoard));
}

TEST(getWinningMove /*unused*/, antidiagonal /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "X", "-"}, {"X", "-", "-"}}};
  Move expectedResult = {0, 2, 0};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getWinningMove /*unused*/, none /*unused*/) {
  GameBoard refBoard = {{{"O", "X", "X"}, {"-", "O", "-"}, {"O", "X", "-"}}};
  Move expectedResult = {0, 0, -1};
  EXPECT_THAT(expectedResult, getWinningMove("X", refBoard));
}

TEST(getBlockingMove /*unused*/, row /*unused*/) {
  GameBoard refBoard = {{{"O", "X", "O"}, {"X", "O", "-"}, {"X", "X", "-"}}};
  Move expectedResult = {2, 2, 0};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(getBlockingMove /*unused*/, col /*unused*/) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {1, 0, 0};
  EXPECT_THAT(expectedResult, getBlockingMove("X", refBoard));
}

TEST(getBlockingMove /*unused*/, diagonal /*unused*/) {
  GameBoard refBoard = {{{"O", "-", "-"}, {"-", "O", "-"}, {"-", "-", "-"}}};
  Move expectedResult = {2, 2, 0};
  EXPECT_THAT(expectedResult, getBlockingMove("X", refBoard));
}

TEST(getBlockingMove /*unused*/, antidiagonal /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "X", "-"}, {"X", "-", "-"}}};
  Move expectedResult = {0, 2, 0};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(getBlockingMove /*unused*/, none /*unused*/) {
  GameBoard refBoard = {{{"O", "X", "X"}, {"-", "O", "-"}, {"O", "X", "-"}}};
  Move expectedResult = {0, 0, -1};
  EXPECT_THAT(expectedResult, getBlockingMove("O", refBoard));
}

TEST(randomMove /*unused*/, emptyBoard /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = randomMove(refBoard);
  EXPECT_LT(result.row, 3);
  EXPECT_LT(result.col, 3);
  EXPECT_EQ(result.state, 0);
}

TEST(randomMove /*unused*/, oneOption /*unused*/) {
  GameBoard refBoard = {{{"X", "X", "-"}, {"O", "X", "O"}, {"X", "O", "O"}}};
  Move expectedResult = {0, 2, 0};
  EXPECT_THAT(expectedResult, randomMove(refBoard));
}

TEST(winMove /*unused*/, prioritizesWin /*unused*/) {
  GameBoard refBoard = {{{"O", "-", "X"}, {"-", "-", "-"}, {"O", "X", "X"}}};
  Move expectedResult = {1, 0, 0};
  EXPECT_THAT(expectedResult, winMove("O", refBoard));
}

TEST(winMove /*unused*/, worksWithoutWin /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = winMove("X", refBoard);
  EXPECT_LT(result.row, 3);
  EXPECT_LT(result.col, 3);
  EXPECT_EQ(result.state, 0);
}

TEST(blockWinMove /*unused*/, prioritizesWin /*unused*/) {
  GameBoard refBoard = {{{"X", "-", "O"}, {"-", "-", "-"}, {"X", "-", "O"}}};
  Move expectedResult = {1, 0, 0};
  EXPECT_THAT(expectedResult, blockWinMove("X", refBoard));
}

TEST(blockWinMove /*unused*/, blocksIfNoWin /*unused*/) {
  GameBoard refBoard = {{{"X", "-", "O"}, {"-", "-", "-"}, {"-", "-", "O"}}};
  Move expectedResult = {1, 2, 0};
  EXPECT_THAT(expectedResult, blockWinMove("X", refBoard));
}

TEST(blockWinMove /*unused*/, worksWithoutBlockWin /*unused*/) {
  GameBoard refBoard = {{{"-", "-", "-"}, {"-", "-", "-"}, {"-", "-", "-"}}};
  Move result = blockWinMove("X", refBoard);
  EXPECT_LT(result.row, 3);
  EXPECT_LT(result.col, 3);
  EXPECT_EQ(result.state, 0);
}
