module TicTacToeHaskell where

import Control.Concurrent (threadDelay)
import Control.Monad (msum)
import Data.Char (isSpace, toUpper)
import Data.List (transpose)
import System.IO (hFlush, stdout)

type Row = [Char]

type Board = [Row]

data EndState = Loss | Draw | Win deriving (Eq, Ord, Show, Read, Bounded, Enum)

negateState :: EndState -> EndState
negateState state
  | state == Loss = Win
  | state == Draw = Draw
  | state == Win = Loss
  | otherwise = Loss

data Move = Move
  { value :: EndState,
    moveRow :: Int,
    moveCol :: Int
  }
  deriving (Show, Eq, Ord)

data FixSpotResult = InvalidInput | OutOfBounds | Taken | Valid deriving (Eq, Ord, Show, Read, Bounded, Enum)

main :: IO ()
main = do
  setting <- getAIOpponent
  start <- if setting then getAIStart else return False
  difficulty <- if setting then getAIStrength else return 4
  playGame setting start difficulty

-- Functionality for playing the game
playGame :: Bool -> Bool -> Int -> IO ()
playGame opponent start difficulty = do
  -- Initialize player char and board
  let player = 'X'
  let board = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
  -- Play the actual game
  newBoard <- takeTurns opponent start difficulty player board
  -- Show the final game board
  showBoard newBoard

-- Game loop of alternating turns
takeTurns :: Bool -> Bool -> Int -> Char -> Board -> IO Board
takeTurns aiOpponent aiTurn aiDifficulty player board = do
  -- Do one turn
  newBoard <- takeTurn aiOpponent aiTurn aiDifficulty player board
  -- Swap the active player and whether it is the AI's turn
  let newPlayer = swapPlayer player
  let newAiTurn = not aiTurn
  -- Check if the game is over. If so end
  -- If not then there will be more turns
  if isWin player newBoard
    then do
      putStrLn $ "Player " ++ [player] ++ " wins the game!"
      return newBoard
    else
      if isDraw newBoard
        then do
          putStrLn "Match Draw!"
          return newBoard
        else do takeTurns aiOpponent newAiTurn aiDifficulty newPlayer newBoard

-- Taking a turn in the current game state
takeTurn :: Bool -> Bool -> Int -> Char -> Board -> IO Board
takeTurn aiOpponent aiTurn aiDifficulty player board
  | aiOpponent && aiTurn = takeAiTurn aiDifficulty player board
  | otherwise = takePlayerTurn player board

-- AI does a turn
takeAiTurn :: Int -> Char -> Board -> IO Board
takeAiTurn difficulty player board = do
  -- Tell the user that it is the AI's turn now
  putStrLn $ "AI turn as " ++ [player] ++ "."
  -- Remind them of the board state
  showBoard board
  hFlush stdout
  threadDelay 1000000
  -- And make it do its move
  return $ doAIMove difficulty player board

-- Get best move and perform it on the board
doAIMove :: Int -> Char -> Board -> Board
doAIMove difficulty player board
  | difficulty == 1 = let Move _ row col = randomMove board in changeBoard player (row, col) board
  | difficulty == 2 = let Move _ row col = winMove player board in changeBoard player (row, col) board
  | difficulty == 3 = let Move _ row col = blockWinMove player board in changeBoard player (row, col) board
  | otherwise = let Move _ row col = minmax player board in changeBoard player (row, col) board

-- Not really a random move at the moment
-- but who cares...
randomMove :: Board -> Move
randomMove board = Move Draw row col
  where
    (row, col) = head (getEmptyCells board)

-- Try to get a winning move for a given line(list of cells)
-- for a player given a board.
getWinningMoveForLine :: Char -> [(Int, Int)] -> Board -> Maybe Move
getWinningMoveForLine player lineIndices board
  | length openMoves == 1 && length doneMoves == 2 = Just (head openMoves)
  | otherwise = Nothing
  where
    doneMoves = [(row, col) | (row, col) <- lineIndices, board !! row !! col == player]
    openMoves = [Move Draw row col | (row, col) <- lineIndices, board !! row !! col == '-']

-- Try to get any winning move for a player given a board by checking each
-- potentially winning line
getAnyWinningMove :: Char -> Board -> Maybe Move
-- msum grabs the first 'Just' value in the list or gives 'Nothing' if there is none
-- msum ((map (getWinningMoveForLine player) listOfWinningLines) <*> [board])
getAnyWinningMove player board = msum listOfMaybeWinningMovesForEachLine
  where
    listOfWinningLines = [[(0, 0), (0, 1), (0, 2)], [(1, 0), (1, 1), (1, 2)], [(2, 0), (2, 1), (2, 2)], [(0, 0), (1, 0), (2, 0)], [(0, 1), (1, 1), (2, 1)], [(0, 2), (1, 2), (2, 2)], [(0, 0), (1, 1), (2, 2)], [(0, 2), (1, 1), (2, 0)]]
    -- Partially apply the player already
    getWinningMoveForLineForPlayer = getWinningMoveForLine player
    -- Get a list of functions that each check for a different line if the given board has a winning move on that line
    listOfgetWinningMoveLineForPlayer = map getWinningMoveForLineForPlayer listOfWinningLines
    -- Apply each of these functions to the board and get a where each entry is Just a winning move (or nothing) for each line on the passed board
    listOfMaybeWinningMovesForEachLine = listOfgetWinningMoveLineForPlayer <*> [board]

-- Try to get any blocking move for a player given a board
getAnyBlockingMove :: Char -> Board -> Maybe Move
-- A blocking move is one that prevents the opponent from making a winning move
getAnyBlockingMove player = getAnyWinningMove (swapPlayer player)

-- Try to perform a winning move
-- if there is none do a random one instead
winMove :: Char -> Board -> Move
winMove player board = case getAnyWinningMove player board of
  Just move -> move
  Nothing -> randomMove board

-- Try to perform a winning or blocking move
-- if there is neither do a random one instead
blockWinMove :: Char -> Board -> Move
blockWinMove player board = case getAnyWinningMove player board of
  Just move -> move
  Nothing -> case getAnyBlockingMove player board of
    Just move -> move
    Nothing -> randomMove board

-- Get the best possible move for the player given the current board
-- via the minmax algorithm
minmax :: Char -> Board -> Move
minmax player board
  | isWin player board = Move Win 0 0
  | isWin (swapPlayer player) board = Move Loss 0 0
  | isDraw board = Move Draw 0 0
  | length emptyCells == 9 = Move Draw 0 0
  | otherwise = myBestMove
  where
    -- Partially apply player so now only the spots and the board are missing
    changeBoardToPlayer = changeBoard player
    -- Grab all empty cells from the board
    emptyCells = getEmptyCells board
    -- Partially apply each empty cell so only the board is missing
    changingEmptyCellsOfBoard = map changeBoardToPlayer emptyCells
    -- Apply each change to the board
    -- Now we have a list of boards where each
    -- is the original board except one empty spot has been changed to the player
    changedBoard = changingEmptyCellsOfBoard <*> [board]
    -- Now for each of these board apply minmax for the opposite player
    -- This gives for each board state the best move that the opponent could do
    bestMovesFromOpponent = map (minmax $ swapPlayer player) changedBoard
    -- Pair up this list of best move for the opponent with our move that lead to that board
    bestMoveOfOpponentWithYourMove = zip bestMovesFromOpponent emptyCells
    -- Find the pair of (best move for the opponent, after this move by me)
    -- That has the worst best move to the opponent
    -- So basically check if there was any board where the opponent loses with every move
    -- Or at least check if there is any move where only a draw was possible
    (worstOpponentMove, (row, col)) = minimum bestMoveOfOpponentWithYourMove
    -- (worstOpponentMove, (row, col)) = minimum $ zip (map (minmax $ swapPlayer player) ((map (changeBoard player) (getEmptyCells board)) <*> [board])) (getEmptyCells board)
    -- Then build our best move from that.
    -- We take the move we made (myRow, myCol) and reverse the EndState that the opponent got.
    -- If they lose that means we won.
    myBestMove = Move {value = negateState $ value worstOpponentMove, moveRow = row, moveCol = col}

-- Get all empty cell on the board
getEmptyCells :: Board -> [(Int, Int)]
getEmptyCells board =
  [ (x, y) -- generate a Coord pair
    | (x, row) <- zip [0 ..] board, -- for each row with its coordinate
      (y, cell) <- zip [0 ..] row, -- for each tile in the row (with coordinate)
      cell == '-' -- if the tile is 1
  ]

-- Actions for a playing taking a turn
takePlayerTurn :: Char -> Board -> IO Board
takePlayerTurn player board = do
  -- Tell the player its their turn
  putStrLn $ "Player " ++ [player] ++ " turn."
  -- Remind them of the board state
  showBoard board
  -- And let them do their turn
  doPlayerMove player board

-- A player making a valid move
doPlayerMove :: Char -> Board -> IO Board
doPlayerMove player board = do
  -- Get the actual input from the human
  input <- getPlayerInput
  -- Try to make the move
  let (valid, newBoard) = fixSpot input player board
  case valid of
    -- If it was a valid move pass the new board up
    Valid -> return newBoard
    -- If not the player has to enter a new move
    OutOfBounds -> do
      putStrLn "Row or column are out of bounds. They have to be between 1 and 3 inclusive. Try again!"
      doPlayerMove player board
    InvalidInput -> do
      putStrLn "Input exactly two integers. Try again!"
      doPlayerMove player board
    Taken -> do
      putStrLn "The position has already been taken by a player! Please do your move on an empty position!"
      doPlayerMove player board

-- Tries to perform a move according to the players input
fixSpot :: [Maybe Int] -> Char -> Board -> (FixSpotResult, Board)
-- Correct player input is exactly two integers
fixSpot [Just input1, Just input2] player board
  -- The move has to be inbounds
  | row < 0 || row > 2 || col < 0 || col > 2 = (OutOfBounds, board)
  -- Move has to happen in an empty space
  | spotTaken row col board = (Taken, board)
  | otherwise = (Valid, changeBoard player (row, col) board)
  where
    -- Input was invalid if there are != inputs or any didnt get parsed

    row = input1 - 1
    col = input2 - 1
fixSpot _ _ board = (InvalidInput, board)

spotTaken :: Int -> Int -> Board -> Bool
-- If the element at position [row][col] is anything but the default then it is taken
spotTaken row col board = board !! row !! col /= '-'

changeBoard :: Char -> (Int, Int) -> Board -> Board
-- Change the given spot on the board to the current player
changeBoard player (row, col) = replace2D player (row, col)

replace :: Int -> (a -> a) -> [a] -> [a]
replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0 ..]]

replace2D :: a -> (Int, Int) -> [[a]] -> [[a]]
replace2D v (x, y) = replace x (replace y (const v))

getPlayerInput :: IO [Maybe Int]
getPlayerInput = do
  putStr "Enter row and column number to fix spot: "
  hFlush stdout
  -- Get the input from the player
  readInts

getAIStrength :: IO Int
getAIStrength = do
  putStrLn "AI strength settings:"
  putStrLn "1: Easy"
  putStrLn "2: Medium"
  putStrLn "3: Hard"
  putStrLn "4: Impossible"
  getAIStrengthFromPlayer

getAIStrengthFromPlayer :: IO Int
getAIStrengthFromPlayer = do
  putStr "How strong should the AI be?[1-4]: "
  hFlush stdout
  input <- readInts
  case input of
    [Just difficulty]
      | difficulty > 0 && difficulty < 5 -> return difficulty
      | otherwise -> getAIStrengthFromPlayer
    _ -> getAIStrengthFromPlayer

-- Try to read every word in the line as Int and produce a list of the results
readInts :: IO [Maybe Int]
readInts = fmap (map maybeRead . words) getLine

-- Try to read a string as type a and produce a maybe of the result
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

isEnd :: Char -> Board -> Bool
isEnd player board
  | isWin player board = True
  | isDraw board = True
  | otherwise = False

isWin :: Char -> Board -> Bool
isWin player board
  -- There is any row where all entries match player
  | any (all (== player)) board = True
  -- There is any column where all entries match player
  | any (all (== player)) (transpose board) = True
  -- All entries in the diagonal match player
  | all (== player) (zipWith (!!) board [0 ..]) = True
  -- ALl entries in the anti diagonal match player
  | all (== player) (zipWith (!!) board [2, 1, 0]) = True
  | otherwise = False

isDraw :: Board -> Bool
-- Game is drawn when no spot is equal to '-' anymore
-- Win has to be checked first
isDraw = all (notElem '-')

swapPlayer :: Char -> Char
swapPlayer 'X' = 'O'
swapPlayer _ = 'X'

-- Functionality for getting the game settings via stdin
-- Get the information whether there should be an AI opponent
getAIOpponent :: IO Bool
getAIOpponent = getYesNo "Play vs AI?[y/n]: "

-- Get information of the AI opponent should make the first move
getAIStart :: IO Bool
getAIStart = getYesNo "Should the AI make the first move?[y/n]: "

getYesNo :: String -> IO Bool
getYesNo question = do
  putStr question
  hFlush stdout
  str <- getLine
  case map toUpper $ trim str of
    "Y" -> return True
    "N" -> return False
    _ -> do
      putStrLn "Invalid input."
      getYesNo question

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- Functionality for printing out the game board
showBoard :: Board -> IO ()
showBoard board = do
  -- Need to put one line separator at the start
  putStrLn "---------------"
  showRows board

-- Then recursively print every row and then a line separator
showRows :: Board -> IO ()
showRows [] = return ()
showRows (row : board) = do
  showRow row
  putStrLn "---------------"
  showRows board

-- Recursively show each element in the row
-- Have a linebreak for the last character in the row
showRow :: Row -> IO ()
showRow [] = return ()
showRow [x] = putStrLn $ showCell x
showRow (x : xs) = do
  putStr $ showCell x
  showRow xs

-- Print each cell
showCell :: Char -> String
showCell x = "| " ++ x : " |"
