module Main where
import OHELLO
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Data.List.Split
import System.Environment
import System.Console.GetOpt
import Text.Read



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
data Flag = Help | Winner | Verbose | Interactive | Depth String | Move String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"]   (NoArg Help)   "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner) "Tells you the best/winning move."
          , Option ['d'] ["depth"]  (ReqArg Depth  "<num>") "Prints the best move as a result of a <d> depth search."
          , Option ['m'] ["move"]   (ReqArg Move "<x, y>") "Makes a move at <x,y>."
          , Option ['v'] ["verbose"]   (NoArg Verbose) "Outputs both the best move and how good it is."
          , Option ['i'] ["interactive"]   (NoArg Interactive) "Runs program in interactive mode."
          ]
   
          
getDepth :: [Flag] -> Int
getDepth ((Depth s):_) = fromMaybe 5 s
getDepth (_:flags) = getDepth flags
getDepth [] = 5

getMove :: [Flag] -> Maybe Location
getMove ((Move s):_) = parseStringNoIO s
getMove (_:flags) = getMove flags
getMove [] = Nothing

parseStringNoIO :: String -> Maybe Location
parseStringNoIO input = loc
    where
        splitted = splitOn "," input
        column = stringToInt $ head splitted
        row = stringToInt $ head $ tail splitted
        loc = (row , column)

main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    let verbose = verbose `elem` flags
    let depth = getDepth args
    let move = getMove 
    if (Help `elem` flags) || (not $ null errors)
        then putStrLn $ usageInfo "Usage: fortunes [options] [file]" options
    else do
        let fileName = if null inputs && (Interactive `notElem` flags) then "intialBoard.txt" else head inputs
                                                                             --TODO:make this
        game <- readGame fileName
        if (Interactive `elem` flags) then --interactive stuff
        else if (Winner `elem` flags)
            then undefined -- call bestestMove not bestMove
        else if move != Nothing then --make the move
        else undefined --regular thing, call bestMove with getDepth flags as the int arg
-- main :: IO ()
-- main = do
--     playGame $ initialGame
--     putStrLn "Game Over"

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
playGame game@(board, turn)= 
    if not(null $ validMoves turn game)
    then do putBoard game
            a <- makeMove game
            playGame $ fromJust a 
    else if not(null $ validMoves (changePlayer turn) game) 
         then do playGame (board, changePlayer turn)
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
