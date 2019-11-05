

data Player = Black | White deriving (Show, Eq) 
data Status = Full Player | Empty deriving (Show, Eq)
type Location = (Integer, Integer)
type Cell = (Location, Status) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)


allDirections = [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]

findCell :: Board -> (Int, Int) -> Cell
findCell cells (x,y) = head [((a,b), status) | ((a,b), status) <- cells, (a == x) && (b == y)]



getAdjacentCells :: Board -> Cell -> [Cell]
getAdjacentCells cells ((x,y), status) = let validDirections = [(a,b) | (a,b) <- allDirections, a+x < 8, b+y <8]
                                                 in []

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

checkWinner :: Board -> Bool
checkWinner = undefined

updateBoard :: Cell -> Board -> Board

updateBoard ((x, y), stat) cellList = if (length checkExists) /= (length cellList)
                                      then flipper ((x,y), stat) (((x,y), stat) : cellList)
                                      else flipper ((x,y), stat) cellList
    where
        checkExists = filter (\((i, j), stati) -> (i /= x) && (j /= y)) cellList

flipper :: Cell -> Board -> Board
flipper ((x, y), stat) cellList = undefined

fancyShow :: Board -> String
fancyShow = undefined

validMoves :: Board -> [Cell]
validMoves = undefined


parseString :: String -> Maybe Cell
parseString = undefined

countPieces :: Player -> Board -> Int
--countPieces counts the number of pieces on the board belonging to a given player.
--It does this by filtering the list of cells in the Board down to only those that 
--we would like to count, then taking the length of that list.
countPieces player cellList = length $ filter (\(location, status) -> status == Full player) cellList

