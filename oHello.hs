

data Player = Black | White
data Status = Player | Empty
type Location = (Int, Int)
type Cell = (Location, Status) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)
type Move = (Location, Player)
--makeBoard :: Player -> Board

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
updateBoard = undefined

fancyShow :: Board -> String
fancyShow = undefined

parseString :: String -> Maybe Cell
parseString = undefined

countPieces :: Player -> Board -> Int
countPieces = undefined
