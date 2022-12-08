module Main where
import System.IO
import Data.Char

type Row = [Char]
type Board = [Row]
type Move = (Int, Int)


main :: IO ()
main =  do
    setting <- getAIOpponent
    start <- if setting then getAISart else return False
    playGame setting start


getAIOpponent :: IO Bool
getAIOpponent = do
          putStr $ "Play vs AI?[y/n]: "
          hFlush stdout
          str <- getLine
          case map toUpper $ trim str of
            "Y" -> return True
            "N" -> return False
            _   -> do
              putStrLn "Invalid input."
              getAIOpponent

getAISart :: IO Bool
getAISart = do
          putStr $ "Should the AI make the first move?[y/n]: "
          hFlush stdout
          str <- getLine
          case map toUpper $ trim str of
            "Y" -> return True
            "N" -> return False
            _   -> do
              putStrLn "Invalid input."
              getAISart

playGame :: Bool -> Bool -> IO ()
playGame opponent start = do
    let player = 'X'
    let board = [['-','-','-'],['-','-','-'],['-','-','-']]
    takeTurns opponent start player board
    showBoard board

takeTurns :: Bool -> Bool -> Char -> Board -> IO()
takeTurns aiOpponent aiTurn player board = do
  takeTurn aiOpponent aiTurn player board
  let newPlayer = swapPlayer player
  let newAiTurn = not aiTurn
  if checkEnd player board then do return () else takeTurns aiOpponent newAiTurn newPlayer board

takeTurn :: Bool -> Bool -> Char -> Board -> IO()
takeTurn aiOpponent aiTurn player board = return ()

checkEnd :: Char -> Board -> Bool
checkEnd player board = True

swapPlayer :: Char -> Char
swapPlayer player = player

showBoard :: Board -> IO ()
showBoard board = do
  putStrLn "---------------"
  showRows board

showRows :: Board -> IO ()
showRows [] = return()
showRows (row:board) = do
  showRow row
  putStrLn "---------------"
  showRows board

showRow  :: Row -> IO ()
showRow [] = return ()
showRow [x] = putStrLn  $ showCell x
showRow (x:xs) = do
    putStr $ showCell x
    showRow xs

showCell :: Char -> String
showCell x = "| " ++ x:" |"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace