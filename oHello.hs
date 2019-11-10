import Data.Char

data Player = Black | White deriving (Show, Eq) 
data Status = Full Player | Empty deriving (Show, Eq)
type Location = (Int, Int) 
type Cell = (Location, Status) --possible wrong syntax
type Board = [Cell] 
type Game = (Board, Player)


allDirections = [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]

findCell :: Board -> (Int, Int) -> Cell
findCell cells (x,y) = head [((a,b), status) | ((a,b), status) <- cells, (a == x) && (b == y)]



getAdjacentCells :: Board -> Cell -> [(Cell, (Int, Int))]
getAdjacentCells cells ((x,y), status) = let validDirections = [(a,b) | (a,b) <- allDirections, a+x < 8, b+y <8]
                                                 in []

otherPlayer :: Player -> Player
otherPlayer Player White = Player Black
otherPlayer Player Black = Player White

{-makeMove :: Game -> Cell -> Maybe Board
makeMove board (loc, Player x) = Nothing
makeMove board ((x,y), Empty) = -}

checkValid :: Board -> Cell -> Bool
checkValid = undefined

findCell :: Board -> Int -> Int -> Cell
findCell = undefined


--checks if game is over
--game is over when both players cannot make a move, or board is full
--true means game is over
gameOver :: Board -> Bool
gameOver board = 
    --undefined
    if length board == 64 then True else False


--if game is over, returns a winner
--else return nothing
checkWinner :: Board -> Maybe Player
checkWinner board = 
    let gameStatus = gameOver board
    in if gameStatus == True then winnerIs board else Nothing

--helper function that calculates winner
--winner is player with most pieces on board
winnerIs :: Board -> Maybe Player
winnerIs board = 
    let 
        lstOfCells = board
        lstOfColors = [colors | (loc, colors) <- lstOfCells]
        count = winnerIsAux lstOfColors
        
    in if count == 0 then Nothing else blackOrWhite count
    
    where
        blackOrWhite count = 
            if count > 0 then Just Black
            else Just White

        --black += 1, white -= 1
        winnerIsAux [] = 0
        winnerIsAux (x:xs) = 
            let recur = winnerIsAux xs
            in if x == Full Black then 1 + recur else (-1) + recur

changeCell :: Maybe Cell -> Board -> Board
--overwrites a single cell on the board.
--VERY DANGEROUS DO NOT CALL WITH POSSIBLY INCORRECT CELLS
changeCell Nothing cellList = cellList
changeCell Just ((x, y), stat) cellList = ((x, y), stat) : [((xi, yi), stati) | ((xi, yi), stati) <- cellList, (xi, yi) /= (x, y)]

updateBoard :: Cell -> Board -> Maybe Board
updateBoard ((x, y), stat) board = 
    let
        adjs = getAdjacentCells ((x, y), stat) board
        rowsToBeFlipped = getRow (board, stat) adjs --[Maybe [Cell]]

        newBoard = recurRowBoardChange rowsToBeFlipped board
    in newBoard


--recurBoardChange gets called by recurRowBoardChange, it's just a pattern-matching recursive function that modifies the board
--for a single list of cells
recurBoardChange :: Maybe [Cell] -> Board -> Board
recurBoardChange Just [] board = board
recurBoardChange Just (cell:s) board = recurBoardChange s (changeCell newCell board)
                               where 
                                newCell = (fst cell, changePlayer $ snd cell)
recurBoardChange Nothing board = board

--recurRowBoardChange is called by updateBoard and calls recurBoardChange. This acts as the first layer of some
--two layer recursion.
recurRowBoardChange :: [Maybe [Cell]] -> Board -> Board
recurRowBoardChange [] board = board
recurRowBoardChange (r:ows) board = recurRowBoardChange ows (recurBoardChange r board)

-- flipper :: Cell -> Board -> Board
-- flipper startCell cellList = recurBoardChange targetCells cellList
--     where
--         adjs = getAdjacentCells cellList startCell
--         targetCells = flatten [sequence $ flipRow dCell (dx, dy) | (dCell, (dx, dy)) <- adjs]
--                                           --switch to new FlipRow function @ 5
type Dir = (Int, Int)
addDir :: Location -> Dir -> Location

fancyShow :: Board -> String
fancyShow = undefined

validMoves :: Board -> [Cell]
validMoves = undefined


parseString :: String -> Maybe Cell
parseString str = 
    let
        column = letterToInt $ head str
        row = digitToInt (head $ tail str)
        loc = (column, row)
        
    in Just (loc, Full White) 

letterToInt :: Char -> Int
letterToInt 'A' = 0
letterToInt 'B' = 1
letterToInt 'C' = 2
letterToInt 'D' = 3
letterToInt 'E' = 4
letterToInt 'F' = 5
letterToInt 'G' = 6
letterToInt 'H' = 7

--help
changePlayer :: Player -> Player
changePlayer (Black) = White
changePlayer (White) = Black


countPieces :: Player -> Board -> Int
--countPieces counts the number of pieces on the board belonging to a given player.
--It does this by filtering the list of cells in the Board down to only those that 
--we would like to count, then taking the length of that list.
countPieces player cellList = length $ filter (\(location, status) -> status == Full player) cellList
