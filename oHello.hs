data Player = Black | White
data Status = Player | Empty
type Cell = (Int, Status) --possible wrong syntax
type Board = ([[Cell]], Player)


checkValid :: Board -> Cell -> Bool

findCell :: Board -> Int -> Int -> Cell

checkWinner :: Board -> Bool

updateBoard :: Cell -> Board -> Board
updateBoard (ind, status) (cellLists, turn) = undefined

fancyShow :: Board -> String

validMoves :: Board -> [Cell]

parseString :: String -> Maybe Cell

countPieces :: Player -> Board -> Int
countPieces Black (cellLists, turn) = undefined