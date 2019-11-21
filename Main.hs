module Main where
import OHELLO
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Data.List.Split




-- /$$$$$$$$ /$$   /$$ /$$$$$$  /$$$$$$        /$$$$$$  /$$$$$$        /$$      /$$  /$$$$$$  /$$$$$$ /$$   /$$      
-- |__  $$__/| $$  | $$|_  $$_/ /$$__  $$      |_  $$_/ /$$__  $$      | $$$    /$$$ /$$__  $$|_  $$_/| $$$ | $$      
--    | $$   | $$  | $$  | $$  | $$  \__/        | $$  | $$  \__/      | $$$$  /$$$$| $$  \ $$  | $$  | $$$$| $$      
--    | $$   | $$$$$$$$  | $$  |  $$$$$$         | $$  |  $$$$$$       | $$ $$/$$ $$| $$$$$$$$  | $$  | $$ $$ $$    
--    | $$   | $$__  $$  | $$   \____  $$        | $$   \____  $$      | $$  $$$| $$| $$__  $$  | $$  | $$  $$$$      
--    | $$   | $$  | $$  | $$   /$$  \ $$        | $$   /$$  \ $$      | $$\  $ | $$| $$  | $$  | $$  | $$\  $$$      
--    | $$   | $$  | $$ /$$$$$$|  $$$$$$/       /$$$$$$|  $$$$$$/      | $$ \/  | $$| $$  | $$ /$$$$$$| $$ \  $$      
--    |__/   |__/  |__/|______/ \______/       |______/ \______/       |__/     |__/|__/  |__/|______/|__/  \__/      
                                                                                                                   
--styleguide for main
 -- big money ne                                                                                                                   
                                                                                                                   
main :: IO ()
main = do
    playGame $ initialGame
    putStrLn "Game Over"

makeMove :: Game -> IO (Maybe Game)
makeMove game@(board, turn) = 
    do 
        str <- prompt $ (playerToString turn) ++ " enter a valid move"
        loc <- parseString str
        let move = (loc, turn)
        let newGame = updateBoard move game
        if newGame == Nothing then makeMove game 
        else return newGame    

playGame :: Game -> IO String
playGame game = 
    if not(null $ validMoves Black game) && not(null $ validMoves White game) then
        do
            putBoard game
            a <- makeMove game
            playGame $ fromJust a 
    else return $ yayWinner game

yayWinner :: Game -> String
yayWinner game = 
    let winner = fromJust $ winnerIs game
    in if winner == Win Black then "Player Black Wins!"
    else                        "Player White Wins!"

prompt :: String -> IO String
prompt message = do putStrLn message
                    a <- getLine
                    let splitted = splitOn "," a
                    let checkSplitted = map validInput splitted
                    if length a /= 3 || False `elem` checkSplitted then prompt message else return a

--ex input "2,4"
--from row:0-7, column:0-7
parseString :: String -> IO Location
parseString str = 
    do
        let splitted = splitOn "," str
        let column = stringToInt $ head splitted
        let row = stringToInt $ head $ tail splitted
        let loc = (row , column)
        return loc
        
validStrs = ["0","1","2","3","4","5","6","7"]

validInput :: String -> Bool
validInput c = c `elem` validStrs

playerToString :: Player -> String
playerToString White = "Player White" 
playerToString Black = "Player Black"

stringToInt :: String -> Int
stringToInt c = 
    read c :: Int

