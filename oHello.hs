import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe

data Player = Black | White deriving (Show, Eq)

type Location = (Int, Int)
type Cell = (Location, Player) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)
type Move = (Location, Player)
type Direction = (Int, Int)
data BST a = Empty | Node a (BST a) (BST a) deriving Show

numRC = [0..7]

allLocs = [(x,y) | x <- numRC, y <- numRC]

allDirections = [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]

showPiece :: Maybe Cell -> String
showPiece (Just (loc, player)) = showPlayer player
showPiece Nothing = "|   |"

showPlayer :: Player -> String
showPlayer Black = "| B |"
showPlayer White = "| W |"

initialBoard = [ ((3::Int,3::Int),White) , ((4::Int,4::Int),White), ((3::Int,4::Int),Black), ((4::Int,3::Int),Black) ]

printRow :: Board -> Int -> String
printRow board no =
             let locs = [ (x,y) | ((x,y),player) <- board]
             in concat [ if (a,b) `elem` locs then showPiece (findCell board (a,b))
                  else showPiece Nothing | (a,b) <- allLocs, a==no]

fancyShow :: Board -> String
fancyShow board=  unlines [printRow board num | num <- numRC]

putBoard :: Board -> IO()
putBoard board = putStr $ fancyShow board
--a "nothing" means the cell doesn't exist

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

--checks if game is over
--game is over when both players cannot make a move, or board is full
--true means game is over
-- gameOver :: Board -> Bool
-- gameOver board =
--     --undefined
--     --option 1: check valid moves available for each player using makeMove or validMoves
--     --if only nothings then true else false
--     let --all = validMovesAll board
--         allWhite = [updateBoard (x, White) board | x <- allLocs]
--         allBlack = [updateBoard (x, Black) board | x <- allLocs]
--     in if gameOverAux allBlack && gameOverAux allWhite then True else False
--     --in if gameOverAux all then True else False

-- gameOverAux :: [Maybe Board] -> Bool
-- gameOverAux [] = True
-- gameOverAux (x:xs) =
--     let recur = gameOverAux xs
--     in if x == Nothing then recur else False


--if game is over, returns a winner
--else return nothing
-- checkWinner :: Board -> Maybe Player
-- checkWinner board =
--     let gameStatus = gameOver board
--     in if gameStatus then winnerIs board else Nothing

--helper function that calculates winner
--winner is player with most pieces on board
--board = [(location, player)]
winnerIs :: Board -> Maybe Player
winnerIs board =
    let numBlack = countPieces Black game
        numWhite = countPieces White game
        
        validMovesWhite = validMoves White game
        validMovesBlack = validMoves Black game        

    in checkWinner numBlack numWhite validMovesBlack validMovesWhite

validMoves :: Player -> Game -> [Location]
--validMoves Black game = [(0,0)]
--validMoves White game = []
validMoves player game = 
    [x | x <- allLocs, updateBoard (x, player) (fst game) /= Nothing]


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
        isValid = (x < 8) && (y < 8) && (x <= 0) && (y <= 0) && ((findCell board (x, y)) == Nothing)
    in if not (null cellsToBeFlipped) && isValid then Just ((((x, y), stat):newBoard), changePlayer stat) else Nothing


--recurBoardChange gets called by recurRowBoardChange, it's just a pattern-matching recursive function that modifies the board
--for a single list of cells, with their colors reversed.
recurBoardChange :: [Cell] -> Board -> Board
recurBoardChange [] board = board
recurBoardChange (cell:s) board = recurBoardChange (s) (changeCell newCell board)
                               where
                                newCell = (fst cell, changePlayer $ snd cell)


--recurRowBoard 11/07/19 - 11/14/19, rip



{-
--ex input "A1"
--from A-H, 1-7
parseString :: String -> Maybe Move
parseString str =
    let
        column = letterToInt $ head str
        row = digitToInt (head $ tail str)
        loc = (column, row)

    in Just (loc, White)

letterToInt :: Char -> Int
letterToInt 'A' = 0
letterToInt 'B' = 1
letterToInt 'C' = 2
letterToInt 'D' = 3
letterToInt 'E' = 4
letterToInt 'F' = 5
letterToInt 'G' = 6
letterToInt 'H' = 7

-}

--help
changePlayer :: Player -> Player
changePlayer (Black) = White
changePlayer (White) = Black


countPieces :: Player -> Game -> Int
--countPieces counts the number of pieces on the board belonging to a given player.
--It does this by filtering the list of cells in the Board down to only those that
--we would like to count, then taking the length of that list.
countPieces player (cellList,turn) = length $ filter (\(location, status) -> status == player) cellList



--  _   _  ______          __ __          ________          _____  ____ _ __      ________
-- | \ | |/ __ \ \        / / \ \        / /  ____|       / ____|/ __ \| |\ \    / /  ____|
-- |  \| | |  | \ \  /\  / /   \ \  /\  / /| |__         | (___ | |  | | | \ \  / /| |__
-- | . ` | |  | |\ \/  \/ /     \ \/  \/ / |  __|         \___ \| |  | | |  \ \/ / |  __|
-- | |\  | |__| | \  /\  / _     \  /\  /  | |____        ____) | |__| | |___\  /  | |____ _
-- |_| \_|\____/   \/  \/ ( )     \/  \/   |______|      |_____/ \____/|______\/   |______(_)
--                       |/


--returns the best next play for the player whose turn it is
bestMove :: Game-> Maybe Cell
bestMove game@(cells, turn) =
                          let valids = validMoves game turn
                              moveVals = getMoveVals game valids
                          in case valids of
                             [] -> Nothing
                             Just v -> bestMove game

getMoveVals :: Game -> [Cell] -> [(Int, Cell)]
getMoveVals game [] = 0
getMoveVals game@(cells, turn) (m:oves) = let newGame = updateBoard m game
                                              cellCount = countPieces turn game
                                          in ((cellCount, m):(getMoveVals game oves))

checkChanges :: Game -> Game -> Int
checkChanges [] [] = 0
checkChanges ((a:as), turn1) ((b:bs), turn2) = if a==b then 1 + checkChanges (as, turn1) (bs, turn2) else checkChanges (as, turn1) (bs, turn2)

allPossibleBoards :: Game -> [Game]
allPossibleBoards (board, turn) = [updateBoard board (cell, turn) | cell <- validMoves board]
