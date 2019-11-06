import Data.Char

data Player = Black | White deriving (Show, Eq)
data Status = Full Player | Empty deriving (Show, Eq)
type Location = (Int, Int)
type Cell = (Location, Status) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)
type Move = (Location, Full Player)
type Direction = (Int, Int)

numRC = [0..7]

allLocs = [(x,y) | x <- numRC, y <- numRC]

allDirections = [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]

instance Show Status where
   show (Full White) = " | W | "
   show (Full Black) = " | B | "
   show (Empty) = " |   | "


initialBoard = [ if loc == (3,3) || loc == (4,4) then (loc,Full White)
                      else if loc == (3,4) || loc == (4,3) then (loc, Full Black)
                      else (loc, Empty)| loc <- allLocs]


findCell :: Board -> (Int, Int) -> Cell
findCell cells (x,y) = head [((a,b), status) | ((a,b), status) <- cells, (a == x) && (b == y)]


--returns list of adjacent cells, each tupled with the direction it is adjacent in
getAdjacentCells :: Board -> Cell -> [(Cell, Direction)]
getAdjacentCells cells ((x,y), status) = let validDirections = [(a,b) | (a,b) <- allDirections, a+x < 8, b+y <8]
                                         in [((a+x,b+y), (a,b)) | (a,b) <- validDirections]

otherPlayer :: Player -> Player
otherPlayer Player White = Player Black
otherPlayer Player Black = Player White

makeMove :: Game -> Cell -> Maybe Board
makeMove board (loc, Player x) = Nothing
makeMove board ((x,y), Empty) =

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


updateBoard :: Cell -> Board -> Board

updateBoard ((x, y), stat) cellList = if (length checkExists) /= (length cellList)
                                      then flipper ((x,y), stat) (((x,y), stat) : cellList)
                                      else flipper ((x,y), stat) cellList
    where
        checkExists = filter (\((i, j), stati) -> (i /= x) && (j /= y)) cellList

flipper :: Cell -> Board -> Board
flipper ((x, y), stat) cellList = undefined

fancyShow :: Board -> String
fancyShow = concat [ show(status) | (loc,status)<-board ]

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

{-
countPieces :: Player -> Board -> Int
--countPieces counts the number of pieces on the board belonging to a given player.
--It does this by filtering the list of cells in the Board down to only those that
--we would like to count, then taking the length of that list.
countPieces player cellList = length $ filter (\(location, status) -> status == Full player) cellList
-}
