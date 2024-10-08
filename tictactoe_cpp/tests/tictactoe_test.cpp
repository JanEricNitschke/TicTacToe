// Copyright 2022-2023 Jan-Eric Nitschke. All rights reserved.

#include <tictactoe.hpp>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <vector>

// Demonstrate some basic assertions.
TEST(createBoardTest /*unused*/, createBoard /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  TicTacToeBoard producedBoard = createBoard<3>();
  EXPECT_EQ(refBoard, producedBoard);
}

TEST(isPlayerWin /*unused*/, emptyBoard /*unused*/) {
  // empty board
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  EXPECT_FALSE(isPlayerWin('X', refBoard));
}

TEST(isPlayerWin /*unused*/, rows /*unused*/) {
  // check rows
  TicTacToeBoard refBoard = {
      {{'X', 'X', 'X'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  EXPECT_TRUE(isPlayerWin('X', refBoard));
  EXPECT_FALSE(isPlayerWin('O', refBoard));
  refBoard = {{{'X', 'X', '-'}, {'-', '-', 'X'}, {'-', '-', '-'}}};
  EXPECT_FALSE(isPlayerWin('X', refBoard));
}

TEST(isPlayerWin /*unused*/, cols /*unused*/) {
  // check cols
  TicTacToeBoard refBoard = {
      {{'X', 'O', 'X'}, {'X', '-', '-'}, {'X', '-', '-'}}};
  EXPECT_TRUE(isPlayerWin('X', refBoard));
  EXPECT_FALSE(isPlayerWin('O', refBoard));
  refBoard = {{{'X', 'O', 'X'}, {'X', '-', '-'}, {'O', '-', '-'}}};
  EXPECT_FALSE(isPlayerWin('X', refBoard));
}

TEST(isPlayerWin /*unused*/, diagonals /*unused*/) {
  // check diagonal
  TicTacToeBoard refBoard = {
      {{'O', 'O', 'X'}, {'X', 'O', '-'}, {'X', '-', 'O'}}};
  EXPECT_TRUE(isPlayerWin('O', refBoard));
  EXPECT_FALSE(isPlayerWin('X', refBoard));
  refBoard = {{{'O', 'O', 'X'}, {'X', 'O', '-'}, {'X', '-', 'X'}}};
  EXPECT_FALSE(isPlayerWin('O', refBoard));

  // check reverse diagonal
  refBoard = {{{'X', 'O', 'O'}, {'-', 'O', 'X'}, {'O', '-', 'X'}}};
  EXPECT_TRUE(isPlayerWin('O', refBoard));
  EXPECT_FALSE(isPlayerWin('X', refBoard));
  refBoard = {{{'X', 'O', 'O'}, {'-', 'O', 'X'}, {'X', '-', 'X'}}};
  EXPECT_FALSE(isPlayerWin('O', refBoard));
}

TEST(isBoardFilled /*unused*/, emptyBoard /*unused*/) {
  // empty board
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
}

TEST(isBoardFilled /*unused*/, filledBoards /*unused*/) {
  // empty board
  TicTacToeBoard refBoard = {
      {{'X', 'X', 'X'}, {'X', 'X', 'X'}, {'X', 'X', 'X'}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{'O', 'O', 'O'}, {'O', 'O', 'O'}, {'O', 'O', 'O'}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{'O', 'X', 'O'}, {'X', 'O', 'O'}, {'O', 'O', 'X'}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
  refBoard = {{{'X', 'O', 'O'}, {'X', 'O', 'O'}, {'X', 'O', 'O'}}};
  EXPECT_TRUE(isBoardFilled(refBoard));
}

TEST(isBoardFilled /*unused*/, nonFilledBoards /*unused*/) {
  // empty board
  TicTacToeBoard refBoard = {
      {{'-', 'X', 'X'}, {'X', 'X', 'X'}, {'X', 'X', 'X'}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{'-', 'O', 'O'}, {'O', '-', 'O'}, {'O', 'O', 'O'}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{'-', 'X', 'O'}, {'-', 'O', 'O'}, {'-', 'O', 'X'}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
  refBoard = {{{'-', 'O', 'O'}, {'X', '-', 'O'}, {'X', 'O', '-'}}};
  EXPECT_FALSE(isBoardFilled(refBoard));
}

TEST(swapPlayer /*unused*/, swappingPlayers /*unused*/) {
  EXPECT_EQ('X', swapPlayer('O'));
  EXPECT_EQ('O', swapPlayer('X'));
}

TEST(getEmptyCells /*unused*/, emptyBoard /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  std::vector<Spot> expectedCells = {{0, 0}, {0, 1}, {0, 2}, {2, 0}, {2, 1},
                                     {2, 2}, {1, 0}, {1, 1}, {1, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells /*unused*/, someEmptyCells /*unused*/) {
  TicTacToeBoard refBoard = {
      {{'X', '-', 'X'}, {'O', 'O', 'O'}, {'-', 'O', '-'}}};
  std::vector<Spot> expectedCells = {{0, 1}, {2, 0}, {2, 2}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
  refBoard = {{{'-', 'X', 'X'}, {'O', 'O', 'O'}, {'X', 'O', 'O'}}};
  expectedCells = {{0, 0}};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(getEmptyCells /*unused*/, fullBoard /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'X', 'O', 'X'}, {'O', 'X', 'O'}, {'X', 'O', 'X'}}};
  std::vector<Spot> expectedCells = {};
  EXPECT_THAT(getEmptyCells(refBoard),
              ::testing::UnorderedElementsAreArray(expectedCells));
}

TEST(minmax /*unused*/, emptyBoard /*unused*/) {
  TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  std::vector<Move> possibleResults = {{
      Move{{0, 0}, GameState::undecided},
      Move{{0, 1}, GameState::undecided},
      Move{{0, 2}, GameState::undecided},
      Move{{1, 0}, GameState::undecided},
      Move{{1, 1}, GameState::undecided},
      Move{{1, 2}, GameState::undecided},
      Move{{2, 0}, GameState::undecided},
      Move{{2, 1}, GameState::undecided},
      Move{{2, 2}, GameState::undecided},
  }};
  EXPECT_THAT(possibleResults, ::testing::Contains(minmax('X', &refBoard)));
}

TEST(minmax /*unused*/, fullBoard /*unused*/) {
  TicTacToeBoard refBoard = {
      {{'X', 'X', 'X'}, {'X', 'X', 'X'}, {'X', 'X', 'X'}}};
  Move expectedResult = {{0, 0}, GameState::win};
  EXPECT_THAT(expectedResult, minmax('X', &refBoard));
  expectedResult = {{0, 0}, GameState::loss};
  EXPECT_THAT(expectedResult, minmax('O', &refBoard));
}

TEST(minmax /*unused*/, partialBoards /*unused*/) {
  TicTacToeBoard refBoard = {
      {{'X', 'X', '-'}, {'O', 'X', 'O'}, {'X', 'O', 'O'}}};
  Move expectedResult = {{0, 2}, GameState::win};
  EXPECT_THAT(expectedResult, minmax('X', &refBoard));
  EXPECT_THAT(expectedResult, minmax('O', &refBoard));
  refBoard = {{{'O', 'O', 'X'}, {'X', '-', 'O'}, {'-', 'O', 'X'}}};
  expectedResult = {{1, 1}, GameState::draw};
  EXPECT_THAT(expectedResult, minmax('X', &refBoard));
  refBoard = {{{'O', 'O', 'X'}, {'X', '-', '-'}, {'-', 'O', 'X'}}};
  expectedResult = {{1, 1}, GameState::win};
  EXPECT_THAT(expectedResult, minmax('O', &refBoard));
  refBoard = {{{'O', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  expectedResult = {{1, 1}, GameState::draw};
  EXPECT_THAT(expectedResult, minmax('X', &refBoard));
}

TEST(getWinningMove /*unused*/, row /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', 'X', 'O'}, {'X', 'O', '-'}, {'X', 'X', '-'}}};
  constexpr Move expectedResult = {{2, 2}, GameState::win};
  EXPECT_THAT(expectedResult, getWinningMove('X', refBoard));
}

TEST(getWinningMove /*unused*/, col /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', '-', '-'}, {'-', '-', '-'}, {'O', 'X', 'X'}}};
  constexpr Move expectedResult = {{1, 0}, GameState::win};
  EXPECT_THAT(expectedResult, getWinningMove('O', refBoard));
}

TEST(getWinningMove /*unused*/, diagonal /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', '-', '-'}, {'-', 'O', '-'}, {'-', '-', '-'}}};
  constexpr Move expectedResult = {{2, 2}, GameState::win};
  EXPECT_THAT(expectedResult, getWinningMove('O', refBoard));
}

TEST(getWinningMove /*unused*/, antidiagonal /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', 'X', '-'}, {'X', '-', '-'}}};
  constexpr Move expectedResult = {{0, 2}, GameState::win};
  EXPECT_THAT(expectedResult, getWinningMove('X', refBoard));
}

TEST(getWinningMove /*unused*/, none /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', 'X', 'X'}, {'-', 'O', '-'}, {'O', 'X', '-'}}};
  constexpr Move expectedResult = {.spot = {.row = 0, .col = 0},
                                   .state = GameState::undecided};
  EXPECT_THAT(expectedResult, getWinningMove('X', refBoard));
}

TEST(getBlockingMove /*unused*/, row /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', 'X', 'O'}, {'X', 'O', '-'}, {'X', 'X', '-'}}};
  constexpr Move expectedResult = {.spot = {.row = 2, .col = 2},
                                   .state = GameState::win};
  EXPECT_THAT(expectedResult, getBlockingMove('O', refBoard));
}

TEST(getBlockingMove /*unused*/, col /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', '-', '-'}, {'-', '-', '-'}, {'O', 'X', 'X'}}};
  constexpr Move expectedResult = {{1, 0}, GameState::win};
  EXPECT_THAT(expectedResult, getBlockingMove('X', refBoard));
}

TEST(getBlockingMove /*unused*/, diagonal /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', '-', '-'}, {'-', 'O', '-'}, {'-', '-', '-'}}};
  constexpr Move expectedResult = {{2, 2}, GameState::win};
  EXPECT_THAT(expectedResult, getBlockingMove('X', refBoard));
}

TEST(getBlockingMove /*unused*/, antidiagonal /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', 'X', '-'}, {'X', '-', '-'}}};
  constexpr Move expectedResult = {{0, 2}, GameState::win};
  EXPECT_THAT(expectedResult, getBlockingMove('O', refBoard));
}

TEST(getBlockingMove /*unused*/, none /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', 'X', 'X'}, {'-', 'O', '-'}, {'O', 'X', '-'}}};
  constexpr Move expectedResult = {{0, 0}, GameState::undecided};
  EXPECT_THAT(expectedResult, getBlockingMove('O', refBoard));
}

TEST(randomMove /*unused*/, emptyBoard /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  Move result = randomMove(refBoard);
  EXPECT_LT(result.spot.row, 3);
  EXPECT_LT(result.spot.col, 3);
  EXPECT_EQ(result.state, GameState::undecided);
}

TEST(randomMove /*unused*/, oneOption /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'X', 'X', '-'}, {'O', 'X', 'O'}, {'X', 'O', 'O'}}};
  constexpr Move expectedResult = {{0, 2}, GameState::undecided};
  EXPECT_THAT(expectedResult, randomMove(refBoard));
}

TEST(winMove /*unused*/, prioritizesWin /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'O', '-', 'X'}, {'-', '-', '-'}, {'O', 'X', 'X'}}};
  constexpr Move expectedResult = {{1, 0}, GameState::win};
  EXPECT_THAT(expectedResult, winMove('O', refBoard));
}

TEST(winMove /*unused*/, worksWithoutWin /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  Move result = winMove('X', refBoard);
  EXPECT_LT(result.spot.row, 3);
  EXPECT_LT(result.spot.col, 3);
  EXPECT_EQ(result.state, GameState::undecided);
}

TEST(blockWinMove /*unused*/, prioritizesWin /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'X', '-', 'O'}, {'-', '-', '-'}, {'X', '-', 'O'}}};
  constexpr Move expectedResult = {{1, 0}, GameState::win};
  EXPECT_THAT(expectedResult, blockWinMove('X', refBoard));
}

TEST(blockWinMove /*unused*/, blocksIfNoWin /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'X', '-', 'O'}, {'-', '-', '-'}, {'-', '-', 'O'}}};
  constexpr Move expectedResult = {{1, 2}, GameState::win};
  EXPECT_THAT(expectedResult, blockWinMove('X', refBoard));
}

TEST(blockWinMove /*unused*/, worksWithoutBlockWin /*unused*/) {
  constexpr TicTacToeBoard refBoard = {
      {{'-', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}};
  Move result = blockWinMove('X', refBoard);
  EXPECT_LT(result.spot.row, 3);
  EXPECT_LT(result.spot.col, 3);
  EXPECT_EQ(result.state, GameState::undecided);
}
