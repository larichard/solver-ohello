module Main where
import Ohello
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
import System.Console.ANSI



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
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    let verbose = Verbose `elem` flags
    let depth = getDepth flags
    let move = getMove flags
    if (Help `elem` flags) || (not $ null errors)
        then putStrLn $ usageInfo "Usage: fortunes [options] [file]" options
    else do
        --setSGR [SetColor Background Dull White]
        --setSGR [Reset]
        let fileName = if null inputs then "initialBoard.txt" else head inputs
        ioGame <-readGame fileName
        let game@(board, turn) = if ioGame == Nothing then error "File does not exist :(" else fromJust ioGame
       
        if (Interactive `elem` flags) --then do 
        --setSGR [SetColor Background Dull Green]
        then playComputer game depth
        --setSGR [Reset]
        else if (Winner `elem` flags)
            then if verbose then verbosePrint (bestestMove game) game
                 else printMove $ bestestMove game
        else if (move /= Nothing) then handleMove game (fromJust move, turn)
        else if verbose then verbosePrint (bestMove game depth) game  else printMove $ bestMove game depth
       -- setSGR [Reset]

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
getDepth ((Depth s):_) = read s
getDepth (_:flags) = getDepth flags
getDepth [] = 5

getMove :: [Flag] -> Maybe Location
getMove ((Move s):_) = if parseStringNoIO s == Nothing then error "Invalid location" else parseStringNoIO s
getMove (_:flags) = getMove flags
getMove [] = Nothing

printBoard :: String ->IO()
printBoard str = putStrLn str --map putStrLn str

putBoard :: Game -> IO()
putBoard game@(board,turn) = do
                         setSGR [SetColor Background Dull Green]
                         setSGR [SetColor Foreground Vivid System.Console.ANSI.White]
                         --putStrLn $ lines (fancyShow game)
                         --putStrLn x | x <- (lines fancyShow game)
                         --printBoard (lines (fancyShow game))
                         putStr $ unlines (lines(fancyShow game))
                         setSGR [Reset]

parseStringNoIO :: String -> Maybe Location
parseStringNoIO input = if check then (Just loc) else Nothing
    where
        splitted = splitOn "," input
        column = stringToInt $ head splitted
        row = stringToInt $ head $ tail splitted
        loc = (row-1 , column-1)
        check = (row > 0) && (row < 9) && (column > 0) && (column < 9)

printMove :: Cell -> IO()
printMove cell@((x,y), player) = putStrLn $ "(" ++ show x ++ "," ++ show (y) ++ ")"

verbosePrint :: Cell -> Game -> IO()
verbosePrint cell game@(board, turn) = do
  printMove cell
  let wins = countWins (getOutcomes cell game) turn
  putStrLn $ "This move has a " ++ show wins ++ " chance of winning."

handleMove :: Game -> Cell -> IO ()
handleMove game@(board, turn) cell  =
    do
        let newGame = updateBoard cell game
        if newGame == Nothing then putStrLn "invalid move"
        else do --setSGR [SetColor Background Dull Green]
                putBoard $ fromJust newGame
                --setSGR [Reset]

makeMove :: Game -> IO (Maybe Game)
makeMove game@(board, turn) =
    do
        str <- prompt $ (playerToString turn) ++ " enter a valid move:"
        loc <- parseString str
        let move = (loc, turn)
        let newGame = updateBoard move game
        if newGame == Nothing then makeMove game
        else return newGame

computerMove :: Game -> Int -> IO (Maybe Game)
computerMove game@(board, turn) depth =
    let move = (fst $ bestMove game depth, turn)
        newGame = updateBoard move game
    in  if newGame == Nothing then makeMove game
        else return newGame

playGame :: Game -> IO String
playGame game@(board, turn)=
    if not(null $ validMoves turn game)
    then do --setSGR [SetColor Background Dull Green]
            putBoard game
            --setSGR [Reset]
            a <- makeMove game
            playGame $ fromJust a
    else if not(null $ validMoves (changePlayer turn) game)
         then do playGame (board, changePlayer turn)
         else return $ yayWinner game

playComputer :: Game -> Int -> IO ()
playComputer game@(board, turn) depth =
    if not(null $ validMoves turn game)
    then do --setSGR [SetColor Background Dull Green]
            putBoard game
            --setSGR [Reset]
            putStrLn $ showCountPieces game
            playerM <- makeMove game
            let newGame@(newBoard, newTurn) = fromJust playerM
            --setSGR [SetColor Background Dull Green]
            putBoard $ fromJust playerM
           -- setSGR [Reset]
            if not(null $ validMoves newTurn newGame) then do
                 setSGR [Reset]
                 putStrLn " "
                 putStrLn "Computer's Turn"
                 compM <- computerMove (fromJust playerM) depth
                 playComputer (fromJust compM) depth
            else putStrLn $ yayWinner game ++ " " ++ showCountPieces game
    else if not(null $ validMoves (changePlayer turn) game)
         then do playComputer (board, changePlayer turn) depth
         else putStrLn $ yayWinner game ++ " " ++ showCountPieces game

compGame = [ ((0::Int,0::Int),Ohello.Black) , ((0::Int,1::Int),Ohello.White), ((1::Int,0::Int),Ohello.White), ((1::Int,1::Int),Ohello.White) ]

showCountPieces :: Game -> String
showCountPieces game = 
    let black = show (countPieces Ohello.Black game)::String
        white = show (countPieces Ohello.White game)::String
    in "Black Pieces: " ++ black ++ ", White Pieces: " ++ white

yayWinner :: Game -> String
yayWinner game =
    let winner = fromJust $ winnerIs game
    in if winner == Win Ohello.Black then "Player Black Wins!"
       else                        "Computer Wins!"

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
        let loc = (row-1 , column-1)
        return loc

validStrs = ["1","2","3","4","5","6","7","8"]

validInput :: String -> Bool
validInput c = c `elem` validStrs

playerToString :: Player -> String
playerToString Ohello.White = "Player White"
playerToString Ohello.Black = "Player Black"

stringToInt :: String -> Int
stringToInt c =
    read c :: Int
