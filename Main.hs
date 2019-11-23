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
        let fileName = if null inputs && (Interactive `notElem` flags) then "initialBoard.txt" else head inputs
        game@(board, turn) <- readGame fileName
        if (Interactive `elem` flags) then playComputer game depth
        else if (Winner `elem` flags)
            then if verbose then verbosePrint $ bestestMove game
                 else printMove $ bestestMove game
        else if (move /= Nothing) then updateBoard (fromJust move, turn) game
        else if verbose then verbosePrint $ bestMove game depth else printMove $ bestMove game depth


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
getMove ((Move s):_) = parseStringNoIO s
getMove (_:flags) = getMove flags
getMove [] = Nothing


parseStringNoIO :: String -> Maybe Location
parseStringNoIO input = if check then (Just loc) else Nothing
    where
        splitted = splitOn "," input
        column = stringToInt $ head splitted
        row = stringToInt $ head $ tail splitted
        loc = (row , column)
        check = (row > 0) && (row < 9) && (column > 0) && (column < 9)


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
        if (Interactive `elem` flags) then undefined--interactive stuff
        else if (Winner `elem` flags)
            then undefined -- call bestestMove not bestMove
        else if move != Nothing then undefined--make the move
        else undefined --regular thing, call bestMove with getDepth flags as the int arg
{-
main :: IO ()
main = do
    playComputer initialGame 8
    putStrLn "Game Over"
-}    




makeMove :: Game -> IO (Maybe Game)
makeMove game@(board, turn) =
    do
        str <- prompt $ (playerToString turn) ++ " enter a valid move"
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
    then do putBoard game
            a <- makeMove game
            playGame $ fromJust a
    else if not(null $ validMoves (changePlayer turn) game)
         then do playGame (board, changePlayer turn)
         else putStrLn $ yayWinner game

playComputer :: Game -> Int -> IO String
playComputer game@(board, turn) depth = 
    if not(null $ validMoves turn game)
    then do putBoard game
            player <- makeMove game
            putBoard $ fromJust player
            putStrLn "Computer's Turn"
            comp <- computerMove (fromJust player) depth
            playComputer (fromJust comp) depth
    else if not(null $ validMoves (changePlayer turn) game) 
         then do playComputer (board, changePlayer turn) depth
         else return $ yayWinner game

compGame = [ ((0::Int,0::Int),Black) , ((0::Int,1::Int),White), ((1::Int,0::Int),White), ((1::Int,1::Int),White) ]


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
