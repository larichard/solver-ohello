import Data.Char

data Player = Black | White deriving (Show, Eq) 
data Status = Full Player | Empty deriving (Show, Eq)
type Location = (Int, Int) 
type Cell = (Location, Status) --possible wrong syntax
type Board = [Cell]
type Game = (Board, Player)
--makeBoard :: Player -> Board


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
updateBoard = undefined

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
countPieces = undefined

