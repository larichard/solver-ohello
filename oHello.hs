data Player = Black | White
data Status = Player | Empty
type Cell = (Int, Status) --possible wrong syntax
type Board = ([[Cell]], Player)
