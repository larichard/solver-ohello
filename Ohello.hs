module Ohello where
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Data.List.Split

data Player = Black | White deriving (Show, Eq)
data Outcome = Tie | Win Player deriving (Show, Eq)

type Location = (Int, Int)
type Cell = (Location, Player) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)
type Direction = (Int, Int)



data BST a = Empty | Node a (BST a) (BST a) deriving Show

-- _   _ _____  _    _       _    _ _____   _____   ___  ___  ___ _____
-- | \ | |  _  || |  | |    | |  | |  ___| |  __ \ / _ \ |  \/  ||  ___|
-- |  \| | | | || |  | |    | |  | | |__   | |  \// /_\ \| .  . || |__
-- | . ` | | | || |/\| |    | |/\| |  __|  | | __ |  _  || |\/| ||  __|
-- | |\  \ \_/ /\  /\  /    \  /\  / |___  | |_\ \| | | || |  | || |____
-- \_| \_/\___/  \/  \  ( )  \/  \/\____/   \____/\_| |_/\_|  |_/\____(_)
--                      |/



numRC = [0..7]

allLocs = [(x,y) | x <- numRC, y <- numRC]

allDirections = [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]

showPiece :: Maybe Cell -> String
showPiece (Just (loc, Black)) = "| B |"
showPiece (Just (loc, White)) = "| W |"
showPiece Nothing = "|   |"

initialBoard = [ ((3::Int,3::Int),White) , ((4::Int,4::Int),White), ((3::Int,4::Int),Black), ((4::Int,3::Int),Black) ]
showValid = "| - |"
printRow :: Game -> Int -> String
printRow game@(board,turn) a = (show (a + 1)) ++ concat [showCell (a,b) | b <- numRC]
    where
        valids = validMoves turn game
        showCell loc = if loc `elem` valids then showValid else showPiece (containsCell board loc) 
topRow = " | 1 || 2 || 3 || 4 || 5 || 6 || 7 || 8 |\n"


fancyShow :: Game -> String
fancyShow game@(board,turn) = topRow ++ unlines [printRow game num | num <- numRC]

putBoard :: Game -> IO()
putBoard game@(board,turn) = putStr $ fancyShow game
    
--a "nothing" means the cell doesn't exist

containsCell :: Board -> (Int, Int) -> Maybe Cell
containsCell cells (x,y) = let poss = [((a,b), status) | ((a,b), status) <- cells, (a == x) && (b == y)]
                       in if null poss then Nothing else Just $ head poss



findCell :: Board -> (Int, Int) -> Maybe Cell
findCell cells (x,y) = let poss = [ ((a,b), status) | ((a,b), status) <- cells, (a == x) && (b == y)]
                       in if null poss then Nothing else Just $ head poss



--returns list of adjacent cells, each tupled with the direction it is adjacent in. if there are no adjacent cells it returns an empty list.
--empty cells are represented by a Nothing tupled with the direction they are in.
getAdjacentCells :: Board -> Cell -> [(Maybe Cell, Direction)]
getAdjacentCells cells ((x,y), status) = let validDirections = [(a,b) | (a,b) <- allDirections, a+x < 8, b+y <8]
                                         in [((findCell cells (a+x,b+y)), (a,b)) | (a,b) <- validDirections]


testBoard = [((0::Int,1::Int), White), ((0::Int,2::Int), White), ((0::Int, 3::Int), White), ((0::Int, 4::Int), Black)]
--returns a row of cells of the non-turn color which ends in a cell of the turn color,
--not including the cell of the turn color. returns Nothing if the row does not
--end in a cell of the turn color (ie the board ends first, or it runs into empty cell first)
getRow :: Game -> Cell -> Direction -> [Cell]
getRow (board, turn) ((x,y), player) (a,b) = let loc = (x+a, y+b)
                                             in fromMaybe [] $ getRowAux (board, turn) loc (a,b)


getRowAux :: Game -> Location -> Direction -> Maybe [Cell]
getRowAux (board, turn) (x,y) (a,b) = let newCell = lookup (x, y) board
                                          restOfRow = getRowAux (board, turn) (x+a,y+b) (a,b)
                                      in case newCell of
                                             Nothing -> Nothing
                                             Just pl -> if pl == turn
                                                        then Just []
                                                        else fmap (((x,y),pl):) restOfRow




--returns next cell in a given direction, or nothing if the board ends
getNext :: Board -> Cell -> Direction -> Maybe Cell
getNext board ((x,y), player) (a,b) = findCell board (x+a, y+b)

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

checkValid :: Board -> Cell -> Bool
checkValid = undefined

--if no moves are available, returns a winner
--if game is not over return nothing
checkWinner :: Int -> Int -> [Location] -> [Location] -> Maybe Outcome
checkWinner blackCount whiteCount validBlack validWhite =
    let movesAreAvailable = null validBlack && null validWhite
    in
      if movesAreAvailable then
        if      blackCount == whiteCount  then Just Tie
        else if blackCount > whiteCount   then Just (Win Black)
        else                                   Just (Win White)
      else
        Nothing

--helper function that calculates winner
--winner is player with most pieces on board
--board = [(location, player)]
winnerIs :: Game -> Maybe Outcome
winnerIs game =
    let
        numBlack = countPieces Black game
        numWhite = countPieces White game

        validMovesWhite = validMoves White game
        validMovesBlack = validMoves Black game

    in checkWinner numBlack numWhite validMovesBlack validMovesWhite


validMoves :: Player -> Game -> [Location]
--validMoves Black game = [(0,0)]
--validMoves White game = []
validMoves player game =
    [x | x <- allLocs, updateBoard (x, player) game /= Nothing]


changeCell :: Cell -> Board -> Board
--overwrites a single cell on the board.
--VERY DANGEROUS DO NOT CALL WITH POSSIBLY INCORRECT CELLS
changeCell (loc, stat) cellList = (loc, stat) : [(loci, stati) | (loci, stati) <- cellList, loci /= loc]

--getRow :: Game -> Cell -> Direction -> Maybe [Cell]
--getAdjacentCells :: Board -> Cell -> [(Maybe Cell, Direction)]
updateBoard :: Cell -> Game -> Maybe Game
updateBoard ((x, y), stat) (board, turn) =
    let cellsToBeFlipped = concat [getRow (board, stat) ((x, y), stat) dir | dir <- allDirections] --[Maybe [Cell]]
        newBoard = recurBoardChange cellsToBeFlipped board
        isValid = (x < 8) && (y < 8) && (x >= 0) && (y >= 0) && ((findCell board (x, y)) == Nothing)
    in if not (null cellsToBeFlipped) && isValid then Just ((((x, y), stat):newBoard), changePlayer stat) else Nothing

--recurBoardChange gets called by recurRowBoardChange, it's just a pattern-matching recursive function that modifies the board
--for a single list of cells, with their colors reversed.
recurBoardChange :: [Cell] -> Board -> Board
recurBoardChange [] board = board
recurBoardChange (cell:s) board = recurBoardChange (s) (changeCell newCell board)
                               where
                                newCell = (fst cell, changePlayer $ snd cell)


--recurRowBoard 11/07/19 - 11/14/19, rip

initialGame = (initialBoard, Black)




--help
changePlayer :: Player -> Player
changePlayer (Black) = White
changePlayer (White) = Black


countPieces :: Player -> Game -> Int
--countPieces counts the number of pieces on the board belonging to a given player.
--It does this by filtering the list of cells in the Board down to only those that
--we would like to count, then taking the length of that list.
countPieces player game = length $ filter (\(location, status) -> status == player) (fst game)


--
--  _   _  _____  _    _      _    _  _____   _____  _____
-- | \ | ||  _  || |  | |    | |  | ||  ___| |_   _||  _  |
-- |  \| || | | || |  | |    | |  | || |__     | |  | | | |
-- | . ` || | | || |/\| |    | |/\| ||  __|    | |  | | | |
-- | |\  |\ \_/ /\  /\  / _  \  /\  /| |___   _| |_ \ \_/ /_
-- \_| \_/ \___/  \/  \ /( )  \/  \/ \____/   \___/  \___/(_)
--                       |/
--
--

customCellShow :: Cell -> String
customCellShow cell@((x, y), player) = (show x) ++ ", " ++ (show y)
--the above and below functions are IO actions that print the best move for a given IO(Maybe Game)
printBestMove :: IO(Maybe Game) -> IO()
printBestMove ioMaybeGame = do
    maybeGame <- ioMaybeGame
    putStrLn $ customCellShow $ bestestMove $ fromJust maybeGame


--this takes a cell and turns it into a string
cellString :: Cell -> String
cellString ((x, y), color) = sX ++ sY ++ sColor ++ "\n"
   where
    sX = show x ++ " "
    sY = show y ++ " "
    sColor = if color == Black then "B " else "W "


--this turns a game into a string
gameToString :: Game -> String
gameToString game@(board, turn) =
    show turn ++ "\n" ++ (concat $ [cellString cell | cell <- board])

--this takes a game, turns it into a string, and then puts said string into a file of your choosing
printToFile :: Game -> String -> IO ()
printToFile gameState filePath = do
    writeFile filePath (gameToString gameState)
{-
(BLACK OR WHITE) TO REPRESENT CURRENT TURN
EACH CELL GETS ITS OWN LINE
FORMAT X Y COLOR
EXAMPLE:
3 3 B
-}

--read game from file!!
readGame :: String -> IO (Maybe Game)
readGame str =
     do
    file <- readFile str
    let lns = lines file
        hd = if head lns == "Black" then Black else White--if head lns== "White" then White else Nothing
        --hd = head lns
    return (Just (doToLines (tail lns), hd))

doToLines :: [String] -> Board
doToLines [] = []
doToLines (x:xs) =
     let dXs = doToLines xs
         first = words x !! 0
         second = words x !! 1
         third = words x !! 2
         cell = if third == "B" then Black else White --if third == "White" then White else ""
         dX = ((read first::Int,read second::Int), cell)
     in (dX:dXs)


testGame = ([((0::Int,0::Int), White), ((0::Int,1::Int), Black)], Black)

finGame = ([(x, White) | x <- allLocs], White)
--finGame = fromJust $ updateBoard ((7,5),Black) fiGame

--  _   _  ______          __   __           ________         _____  ____ _ __      ________
-- | \ | |/ __ \ \        / /   \ \        / /  ____|       / ____|/ __ \| |\ \    / /  ____|
-- |  \| | |  | \ \  /\  / /     \ \  /\  / /| |__         | (___ | |  | | | \ \  / /| |__
-- | . ` | |  | |\ \/  \/ /       \ \/  \/ / |  __|         \___ \| |  | | |  \ \/ / |  __|
-- | |\  | |__| | \  /\  / _       \  /\  /  | |____        ____) | |__| | |___\  /  | |____ _
-- |_| \_|\____/   \/  \/ ( )       \/  \/   |______|      |_____/ \____/|______\/   |______(_)
--                        |/



-- returns the best next play for the player whose turn it is
-- bestMove :: Game-> Cell
-- bestMove game@(cells, turn) =
--                           let valids = validMoves turn game
--                           in case valids of
--                              [] -> if validMoves (changePlayer turn) (cells, (changePlayer turn)) == [] then
--                                    else bestMove (cells,(changePlayer turn))
--                               v -> maximum [bestMove (updateBoard cell game) | cell <- valids]

superSafeIO :: IO (Maybe Game) -> Game
--this is fine
superSafeIO dumb = fromJust $ unsafePerformIO dumb

readFileToGame :: String -> Game
readFileToGame file = superSafeIO $ readGame file

bestestMove :: Game -> Cell
bestestMove game@(cells, turn) =
  let valids = validMoves turn game
      outcomes = [((c,turn) , getOutcomes (c, turn) game) | c <- valids]
  in ranker outcomes

bestMove :: Game -> Int -> Cell
bestMove game@(cells, turn) d =
    let valids = validMoves turn game
        outcomes = [((c,turn) , getDLevel (c, turn) game d) | c <- valids]
    in ranker outcomes

getDLevel :: Cell -> Game -> Int -> [Outcome]
getDLevel cell game@(cells, turn) 0 =
    if blackCount == whiteCount then  [Tie]
    else if blackCount > whiteCount then [Win Black]
    else [Win White]
    where
        blackCount = countPieces Black game
        whiteCount = countPieces White game
getDLevel cell game@(cells, turn) d =
    let newGame@(newBoard, nextTurn) = fromMaybe ([], (changePlayer turn)) (updateBoard cell game)
        valids = validMoves nextTurn newGame
    in if winnerIs newGame == Nothing then concat [getDLevel (c, turn) newGame (d-1) | c <- valids]
    else [fromJust (winnerIs newGame)]

getOutcomes :: Cell -> Game -> [Outcome]
getOutcomes cell game@(cells, turn) =
  let newGame@(newBoard, nextTurn) = fromMaybe ([], (changePlayer turn)) (updateBoard cell game)
      valids = validMoves nextTurn newGame
  in if winnerIs newGame == Nothing then concat [getOutcomes (c, turn) newGame | c <- valids]
     else [fromJust (winnerIs newGame)]

ranker :: [(Cell, [Outcome])] -> Cell
ranker [x] = fst x
ranker ((cell@(loc,player), outcomes):(cell2, outcomes2):xs)  =
   if countWins outcomes player >  countWins outcomes2 player then ranker ((cell,outcomes):xs)
   else ranker ((cell2, outcomes2):xs)

countWins :: [Outcome] -> Player -> Double
countWins outcomes turn = (sum (map (\outcome -> if outcome == (Win turn) then 1.0 else 0.0) outcomes)) / fromIntegral (length outcomes)



-- getMoveVals :: Game -> [Cell] -> [(Int, Cell)]
-- getMoveVals game [] = 0
-- getMoveVals game@(cells, turn) (m:oves) = let newGame = updateBoard m game
--                                               cellCount = countPieces turn game
--                                           in ((cellCount, m):(getMoveVals game oves))
--
-- checkChanges :: Game -> Game -> Int
-- checkChanges [] [] = 0
-- checkChanges [x] [] = 1
-- checkChanges [] [x] = 1
-- -- checkChanges ((a:as), turn1) ((b:bs), turn2) = if a==b then 1 + checkChanges (as, turn1) (bs, turn2) else checkChanges (as, turn1) (bs, turn2)
--
-- allPossibleBoards :: Game -> [Game]
-- allPossibleBoards (board, turn) = [updateBoard board (cell, turn) | cell <- validMoves board]
