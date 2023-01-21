module Main where

import Data.List
import Test.HUnit
import TicTacToeHaskell

testEndState :: Test
testEndState =
  TestCase
    ( do
        assertEqual "Negating Win to Loss" Loss (negateState Win)
        assertEqual "Negating Draw to Draw" Draw (negateState Draw)
        assertEqual "Negating Loss to Win" Win (negateState Loss)
    )

testMove :: Test
testMove =
  TestCase
    ( do
        let myMove1 = Move Loss 1 2
        let myMove2 = Move Draw 2 1
        let myMove3 = Move Loss 1 2
        let myMove4 = Move Loss 2 1
        let myMove5 = Move Win 0 0
        assertEqual "First criteria is EndState and Loss < Draw" LT (myMove1 `compare` myMove2)
        assertEqual "All entries equal" EQ (myMove1 `compare` myMove3)
        assertEqual "Second criteria is row and 1 < 2" LT (myMove1 `compare` myMove4)
        assertEqual "Loss < Win" LT (myMove1 `compare` myMove5)
        assertEqual "Draw > Loss" GT (myMove2 `compare` myMove3)
        assertEqual "Draw > Loss" GT (myMove2 `compare` myMove4)
        assertEqual "Draw < Win" LT (myMove2 `compare` myMove5)
        assertEqual "1 < 2" LT (myMove3 `compare` myMove4)
        assertEqual "Loss < Win" LT (myMove3 `compare` myMove5)
        assertEqual "Loss < Win" LT (myMove4 `compare` myMove5)
    )

testminmaxFullBoard :: Test
testminmaxFullBoard =
  TestCase
    ( do
        let board1 = [['X', 'X', 'X'], ['X', 'X', 'X'], ['X', 'X', 'X']]
        let Move myEndState1 _ _ = minmax 'X' board1
        let Move myEndState2 _ _ = minmax 'O' board1
        assertEqual "Best move on won board has endstate win" Win myEndState1
        assertEqual "Best move on lost board has endstate loss" Loss myEndState2
        let board2 = [['X', 'O', 'X'], ['O', 'X', 'O'], ['O', 'X', 'O']]
        let Move myEndState3 _ _ = minmax 'X' board2
        let Move myEndState4 _ _ = minmax 'O' board2
        assertEqual "Best move on drawn board has endstate draw" Draw myEndState3
        assertEqual "Best move on drawn board has endstate draw" Draw myEndState4
    )

testminmaxBestMove :: Test
testminmaxBestMove =
  TestCase
    ( do
        let board1 = [['X', 'X', '-'], ['O', 'X', 'O'], ['X', 'O', 'O']]
        assertEqual "Correctly does only available move and gets correct result (X case, row)" (Move Win 0 2) (minmax 'X' board1)
        assertEqual "Correctly does only available move and gets correct result (O case, column)" (Move Win 0 2) (minmax 'O' board1)
        let board2 = [['O', 'O', 'X'], ['X', '-', 'O'], ['-', 'O', 'X']]
        assertEqual "Correctly blocks column and forces draw" (Move Draw 1 1) (minmax 'X' board2)
        let board3 = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']]
        assertEqual "Correctly chooses winning move out of three possibly" (Move Win 1 1) (minmax 'O' board3)
        let board4 = [['X', '-', 'X'], ['O', '-', 'A'], ['X', 'A', 'O']]
        let Move myEndState1 _ _ = minmax 'O' board4
        assertEqual "Correctly determines loss in split situation" Loss myEndState1
        let board5 = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', 'O']]
        let Move myEndState2 _ _ = minmax 'X' board5
        assertEqual "Correctly determines win when setting up split is possible" Win myEndState2
        let board6 = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        assertEqual "Correctly blocks middle after opponent opens with corner" (Move Draw 1 1) (minmax 'O' board6)
    )

testgetAnyWinningMoveFindsWinningMove :: Test
testgetAnyWinningMoveFindsWinningMove =
  TestCase
    ( do
        let board1 = [['O', 'X', 'O'], ['X', 'O', '-'], ['X', 'X', '-']]
        assertEqual "Correctly finds winning move in row" (Just $ Move Draw 2 2) (getAnyWinningMove 'X' board1)
        let board2 = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']]
        assertEqual "Correctly finds winning move in col" (Just $ Move Draw 1 0) (getAnyWinningMove 'O' board2)
        let board3 = [['O', '-', '-'], ['-', 'O', '-'], ['-', '-', '-']]
        assertEqual "Correctly finds winning move in diagonal" (Just $ Move Draw 2 2) (getAnyWinningMove 'O' board3)
        let board4 = [['-', '-', '-'], ['-', 'X', '-'], ['X', '-', '-']]
        assertEqual "Correctly finds winning move in antidiagonal" (Just $ Move Draw 0 2) (getAnyWinningMove 'X' board4)
    )

testgetAnyWinningMoveNoWinner :: Test
testgetAnyWinningMoveNoWinner =
  TestCase
    ( do
        let board1 = [['O', 'X', 'X'], ['-', 'O', '-'], ['O', 'X', '-']]
        assertEqual "Correctly finds no winning move" Nothing (getAnyWinningMove 'X' board1)
    )

testgetAnyBlockingMoveFindsWinningMove :: Test
testgetAnyBlockingMoveFindsWinningMove =
  TestCase
    ( do
        let board1 = [['O', 'X', 'O'], ['X', 'O', '-'], ['X', 'X', '-']]
        assertEqual "Correctly finds blocking move in row" (Just $ Move Draw 2 2) (getAnyBlockingMove 'O' board1)
        let board2 = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']]
        assertEqual "Correctly finds blocking move in col" (Just $ Move Draw 1 0) (getAnyBlockingMove 'X' board2)
        let board3 = [['O', '-', '-'], ['-', 'O', '-'], ['-', '-', '-']]
        assertEqual "Correctly finds blocking move in diagonal" (Just $ Move Draw 2 2) (getAnyBlockingMove 'X' board3)
        let board4 = [['-', '-', '-'], ['-', 'X', '-'], ['X', '-', '-']]
        assertEqual "Correctly finds blocking move in antidiagonal" (Just $ Move Draw 0 2) (getAnyBlockingMove 'O' board4)
    )

testgetAnyBlockingMoveNoWinner :: Test
testgetAnyBlockingMoveNoWinner =
  TestCase
    ( do
        let board1 = [['O', 'X', 'X'], ['-', 'O', '-'], ['O', 'X', '-']]
        assertEqual "Correctly finds no blocking move" Nothing (getAnyBlockingMove 'O' board1)
    )

testrandMoveEmptyBoard :: Test
testrandMoveEmptyBoard =
  TestCase
    ( do
        let board1 = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        let Move state row col = randomMove board1
        assertBool "randomMove is in bounds" (row >= 0 && row < 3 && col >= 0 && col < 3)
        assertEqual "EndState is given as draw" Draw state
    )

testrandomMoveOneSpot :: Test
testrandomMoveOneSpot =
  TestCase
    ( do
        let board1 = [['X', 'X', '-'], ['O', 'X', 'O'], ['X', 'O', 'O']]
        assertEqual "randomMove picks only valid spot" (Move Draw 0 2) (randomMove board1)
    )

testwinMoveWithWin :: Test
testwinMoveWithWin =
  TestCase
    ( do
        let board1 = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']]
        assertEqual "winMove prioritizes win" (Move Draw 1 0) (winMove 'O' board1)
    )

testwinMoveWithOutWin :: Test
testwinMoveWithOutWin =
  TestCase
    ( do
        let board1 = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        let Move state row col = winMove 'X' board1
        assertBool "Random fallback is in bounds" (row >= 0 && row < 3 && col >= 0 && col < 3)
        assertEqual "Random fallback state is given as draw" Draw state
    )

testblockWinMoveWithWin :: Test
testblockWinMoveWithWin =
  TestCase
    ( do
        let board1 = [['X', '-', 'O'], ['-', '-', '-'], ['X', '-', 'O']]
        assertEqual "blockWinMove prioritizes win" (Move Draw 1 0) (blockWinMove 'X' board1)
    )

testblockWinMoveWithBlock :: Test
testblockWinMoveWithBlock =
  TestCase
    ( do
        let board1 = [['X', '-', 'O'], ['-', '-', '-'], ['-', '-', 'O']]
        assertEqual "blockWinMove blocks if there is no win" (Move Draw 1 2) (blockWinMove 'X' board1)
    )

testblockWinMoveWithOutWin :: Test
testblockWinMoveWithOutWin =
  TestCase
    ( do
        let board1 = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        let Move state row col = blockWinMove 'X' board1
        assertBool "Random fallback is in bounds" (row >= 0 && row < 3 && col >= 0 && col < 3)
        assertEqual "Random fallback state is given as draw" Draw state
    )

-- Helper function to check if two lists contain the same elements
identicalElements :: (Eq a) => [a] -> [a] -> Bool
identicalElements x y = null (x \\ y) && null (y \\ x)

testListHelper :: Test
testListHelper =
  TestCase
    ( do
        assertBool "Identical lists contain the same elements" (identicalElements ([1, 2, 3, 4] :: [Int]) ([1, 2, 3, 4] :: [Int]))
        assertBool "Shuffeled lists contain the same elements" (identicalElements ([1, 2, 3, 4] :: [Int]) ([4, 1, 3, 2] :: [Int]))
        assertBool "Differently sized lists do not contain the same elements" (not (identicalElements ([1, 2, 3, 4] :: [Int]) ([1, 2, 3] :: [Int])))
        assertBool "Different lists do not contain the same elements" (not (identicalElements ([1, 2, 3, 4] :: [Int]) ([1, 2, 3, 5] :: [Int])))
    )

testgetEmptyCells :: Test
testgetEmptyCells =
  TestCase
    ( do
        let emptyBoard = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        assertBool "All cells are missing on an empty board" (identicalElements [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)] (getEmptyCells emptyBoard))
        let fullBoard = [['X', 'O', 'O'], ['O', 'X', 'O'], ['O', 'X', 'X']]
        assertEqual "No cells are missing on a full board -> empty list" [] (getEmptyCells fullBoard)
        let partialBoard = [['X', 'O', '-'], ['O', '-', 'X'], ['-', 'X', 'X']]
        assertBool "Gets correct empty cells from partially filled board" (identicalElements [(0, 2), (2, 0), (1, 1)] (getEmptyCells partialBoard))
    )

testfixSpotInvalidUserInput :: Test
testfixSpotInvalidUserInput =
  TestCase
    ( do
        let board1 = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']]
        let (status1, _) = fixSpot [Nothing, Just 2] 'X' board1
        assertEqual "Any invalid read means an invalid fix spot" InvalidInput status1
        let (status2, _) = fixSpot [Just 1] 'X' board1
        assertEqual "Too few inputs (<2) are invalid" InvalidInput status2
        let (status3, _) = fixSpot [Just 1, Just 2, Just 3] 'X' board1
        assertEqual "Too many inputs (>2) are invalid" InvalidInput status3
    )

testfixSpotValidUserInput :: Test
testfixSpotValidUserInput =
  TestCase
    ( do
        let board1 = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']]
        let (status1, _) = fixSpot [Just 1, Just 2] 'X' board1
        assertEqual "Recognizes taken spot" Taken status1
        let (status2, _) = fixSpot [Just 0, Just 6] 'X' board1
        assertEqual "Recognizes out of bounds input" OutOfBounds status2
        let (status3, newBoard) = fixSpot [Just 2, Just 3] 'X' board1
        let expectedBoard = [['O', 'O', 'X'], ['X', '-', 'X'], ['-', 'O', 'X']]
        assertEqual "Correctly fixes spot on valid input" (Valid, expectedBoard) (status3, newBoard)
    )

testspotTaken :: Test
testspotTaken =
  TestCase
    ( do
        let board1 = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']]
        assertBool "Recognizes taken spot" (spotTaken 2 1 board1)
        assertBool "Recognizes empty spot" (not $ spotTaken 1 1 board1)
    )

testchangeBoard :: Test
testchangeBoard =
  TestCase
    ( do
        let list1 = [1, 2, 3] :: [Int]
        assertEqual "Replaces in 1-D" ([1, 2, 5] :: [Int]) (replace 2 (const 5) list1)
        let list2 = [[1, 2, 3], [4, 5, 6]]
        assertEqual "Replaces in 2-D" ([[1, 2, 3], [4, 5, 9]] :: [[Int]]) (replace2D 9 (1, 2) list2)
        let board1 = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']]
        let expectedBoard = [['O', 'O', 'X'], ['X', '-', 'O'], ['-', 'O', 'X']]
        assertEqual "Change board works" expectedBoard (changeBoard 'O' (1, 2) board1)
    )

testisWinNoWinner :: Test
testisWinNoWinner =
  TestCase
    ( do
        let emptyBoard = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        assertBool "No one wins on an empty board (X case)" (not $ isWin 'X' emptyBoard)
        assertBool "No one wins on an empty board (O case)" (not $ isWin 'O' emptyBoard)
    )

testisWinRow :: Test
testisWinRow =
  TestCase
    ( do
        let rowBoardX = [['X', 'X', 'X'], ['-', '-', '-'], ['-', '-', '-']]
        assertBool "X wins on full row" (isWin 'X' rowBoardX)
        assertBool "O does not win on X full row" (not $ isWin 'O' rowBoardX)
        let rowBoardO = [['X', 'X', 'O'], ['-', '-', '-'], ['O', 'O', 'O']]
        assertBool "O wins on full row" (isWin 'O' rowBoardO)
        assertBool "X does not win on O full row" (not $ isWin 'X' rowBoardO)
    )

testisWinCol :: Test
testisWinCol =
  TestCase
    ( do
        let colBoardX = [['X', '-', '-'], ['X', '-', '-'], ['X', '-', '-']]
        assertBool "X wins on full col" (isWin 'X' colBoardX)
        assertBool "O does not win on X full col" (not $ isWin 'O' colBoardX)
        let colBoardO = [['X', 'X', 'O'], ['-', '-', 'O'], ['X', 'O', 'O']]
        assertBool "O wins on full col" (isWin 'O' colBoardO)
        assertBool "X does not win on O full col" (not $ isWin 'X' colBoardO)
    )

testisWinDiag :: Test
testisWinDiag =
  TestCase
    ( do
        let diagBoardMain = [['X', '-', '-'], ['-', 'X', '-'], ['O', '-', 'X']]
        assertBool "X wins on full diagonal" (isWin 'X' diagBoardMain)
        assertBool "O does not win on X full diagonal" (not $ isWin 'O' diagBoardMain)
        let diagBoardAnti = [['X', 'X', 'O'], ['-', 'O', 'X'], ['O', 'X', 'O']]
        assertBool "X wins on full anti diagonal" (isWin 'O' diagBoardAnti)
        assertBool "O does not win on X full anti diagonal" (not $ isWin 'X' diagBoardAnti)
    )

testisDraw :: Test
testisDraw =
  TestCase
    ( do
        let emptyBoard = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        assertBool "Empty board is not draw" (not $ isDraw emptyBoard)
        let partialBoard1 = [['X', '-', '-'], ['-', 'X', '-'], ['O', '-', 'X']]
        assertBool "Partial board1 is not draw" (not $ isDraw partialBoard1)
        let partialBoard2 = [['X', 'X', 'O'], ['-', 'X', 'X'], ['O', 'O', 'X']]
        assertBool "Partial board2 is not draw" (not $ isDraw partialBoard2)
        let fullBoard1 = [['X', 'X', 'X'], ['X', 'X', 'X'], ['O', 'X', 'X']]
        assertBool "Full board1 is draw" (isDraw fullBoard1)
        let fullBoard2 = [['X', 'X', 'O'], ['O', 'X', 'X'], ['O', 'O', 'X']]
        assertBool "Full board2 is not draw" (isDraw fullBoard2)
    )

testisEnd :: Test
testisEnd =
  TestCase
    ( do
        let emptyBoard = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        assertBool "Empty board is not end" (not $ isEnd 'X' emptyBoard)
        assertBool "Empty board is not end" (not $ isEnd 'O' emptyBoard)
        let partialBoard1 = [['X', 'O', 'O'], ['O', '-', 'O'], ['O', 'X', 'X']]
        assertBool "Partial board without winner is not end" (not $ isEnd 'X' partialBoard1)
        assertBool "Partial board without winner is not end" (not $ isEnd 'O' partialBoard1)
        let partialBoard2 = [['X', 'X', 'O'], ['-', '-', '-'], ['O', 'O', 'O']]
        assertBool "Partial board with wrong player is not end" (not $ isEnd 'X' partialBoard2)
        assertBool "Partial board with correct player is end" (isEnd 'O' partialBoard2)
        let fullBoard = [['X', 'O', 'O'], ['X', 'O', 'O'], ['O', 'X', 'X']]
        assertBool "Full board with correct winner is end" (isEnd 'O' fullBoard)
        assertBool "Full board without winner is end" (isEnd 'X' fullBoard)
    )

testswapPlayer :: Test
testswapPlayer =
  TestCase
    ( do
        assertEqual "Swaps O to X" 'X' (swapPlayer 'O')
        assertEqual "Swaps X to O" 'O' (swapPlayer 'X')
    )

testtrimHelper :: Test
testtrimHelper =
  TestCase
    ( do
        assertEqual "Trims nothing" "ABC" (trim "ABC")
        assertEqual "Trims front" "ABC" (trim "   ABC")
        assertEqual "Trims back" "ABC" (trim "ABC    ")
        assertEqual "Trims both" "ABC" (trim "       ABC ")
    )

testshowCell :: Test
testshowCell =
  TestCase
    ( do
        assertEqual "Show cells works for X" "| X |" (showCell 'X')
        assertEqual "Show cells works for O" "| O |" (showCell 'O')
    )

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        [ TestLabel "Testing EndState negation" testEndState,
          TestLabel "Testing Move comparison" testMove,
          TestLabel "Testing that minmax determines the correct final state on a full board" testminmaxFullBoard,
          TestLabel "Testing that minmax determines correct move/outcome in open situations" testminmaxBestMove,
          TestLabel "Same element list helper function works" testListHelper,
          TestLabel "Get empty cells gets the correct fields for empty, full and partially filled board" testgetEmptyCells,
          TestLabel "Fixspot correctly handles invalid user input" testfixSpotInvalidUserInput,
          TestLabel "Fixspot correctly handles valid user input" testfixSpotValidUserInput,
          TestLabel "Spot taken works" testspotTaken,
          TestLabel "Change board along with its helper functions works" testchangeBoard,
          TestLabel "isWin works when there is no winner" testisWinNoWinner,
          TestLabel "isWin recognizes win by row" testisWinRow,
          TestLabel "isWin recognizes win by col" testisWinCol,
          TestLabel "isWin recognizes win by diagonals" testisWinDiag,
          TestLabel "isDraw works on empty, partial and full board" testisDraw,
          TestLabel "isEnd works in both win and draw situations" testisEnd,
          TestLabel "Swap player works" testswapPlayer,
          TestLabel "Trim works in all configurations" testtrimHelper,
          TestLabel "Show cell works for both player" testshowCell,
          TestLabel "getAnyWinningMove finds a winning move if one is available" testgetAnyWinningMoveFindsWinningMove,
          TestLabel "getAnyWinningMove finds nothing if no winning move is available" testgetAnyWinningMoveNoWinner,
          TestLabel "getAnyBlockingMove finds a winning move if one is available" testgetAnyBlockingMoveFindsWinningMove,
          TestLabel "getAnyBlockingMove finds nothing if no winning move is available" testgetAnyBlockingMoveNoWinner,
          TestLabel "randomMove behaves within bounds" testrandMoveEmptyBoard,
          TestLabel "randomMove picks valid spot" testrandomMoveOneSpot,
          TestLabel "winMove finds win if possible" testwinMoveWithWin,
          TestLabel "winMove still gives a move without win" testwinMoveWithOutWin,
          TestLabel "blockWinMove finds win if possible" testblockWinMoveWithWin,
          TestLabel "blockWinMove finds win if possible" testblockWinMoveWithBlock,
          TestLabel "blockWinMove still gives a move without win" testblockWinMoveWithOutWin
        ]
    )
