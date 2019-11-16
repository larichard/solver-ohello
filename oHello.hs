import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe

data Player = Black | White deriving (Show, Eq)
data Outcome = Tie | Full Player deriving (Show)

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
showPiece (Just (loc, Black)) = "| B |"
showPiece (Just (loc, White)) = "| W |"
showPiece Nothing = "|   |"

initialBoard = [ ((3::Int,3::Int),White) , ((4::Int,4::Int),White), ((3::Int,4::Int),Black), ((4::Int,3::Int),Black) ]

printRow :: Board -> Int -> String
printRow board no = concat [ showPiece (containsCell board (a,b)) | (a,b) <- allLocs, a==no]

fancyShow :: Board -> String
fancyShow board=  unlines [printRow board num | num <- numRC]

putBoard :: Board -> IO()
putBoard board = putStr $ fancyShow board
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

--if no moves are available, returns a winner
--if game is not over return nothing
checkWinner :: Int -> Int -> [Location] -> [Location] -> Maybe Outcome
checkWinner blackCount whiteCount validBlack validWhite =
    let movesAreAvailable = null validBlack && null validWhite 
    in
      if movesAreAvailable then
        if      blackCount == whiteCount  then Just Tie
        else if blackCount > whiteCount   then Just (Full Black)
        else                                   Just (Full White)
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
    [x | x <- allLocs, updateBoard (x, player) (game) /= Nothing]

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
countPieces player game = length $ filter (\(location, status) -> status == player) (fst game)


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

--turnToCell :: 



testGame = ([((0::Int,0::Int), White), ((0::Int,1::Int), Black)], Black)
testGameFull = (fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++
                fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++
                fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++
                fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) ++ fst(testGame) , Black)

-- #     # ####### #     #         #     # #######     #####  ####### #       #     # #######     
-- ##    # #     # #  #  #         #  #  # #          #     # #     # #       #     # #           
-- # #   # #     # #  #  #         #  #  # #          #       #     # #       #     # #           
-- #  #  # #     # #  #  #         #  #  # #####       #####  #     # #       #     # #####       
-- #   # # #     # #  #  #  ###    #  #  # #                # #     # #        #   #  #         
-- #    ## #     # #  #  #  ###    #  #  # #          #     # #     # #         # #   #        ## 
-- #     # #######  ## ##    #      ## ##  #######     #####  ####### #######    #    #######  ## 
--                          #                                                                     



-- allPossibleBoards :: Game -> [Game]
-- allPossibleBoards (board, turn) = [updateBoard board (cell, turn) | cell <- validMoves board]


