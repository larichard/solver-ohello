

data Player = Black | White
data Status = Player | Empty
type Location = (Int, Int)
type Cell = (Location, Status) --possible wrong syntax
type Board = ([Cell], Player)
type Game = (Board, Player)
--makeBoard :: Player -> Board


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

validMoves :: Board -> [Cell]
validMoves = undefined

parseString :: String -> Maybe Cell
parseString = undefined

countPieces :: Player -> Board -> Int
countPieces = undefined
