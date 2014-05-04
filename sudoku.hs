import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Debug.Trace

-- UTILITY
(/>) = flip ($)

subArr xs indices = map (xs!!) indices


data Cell = Cell {
	options :: [Int],
	value :: Maybe Int,
	position :: (Int, Int)
} deriving (Show, Eq)

data Grid = Grid {
	cells :: [[Cell]]
} deriving (Show, Eq)



-- CELL
newCell :: (Maybe Int) -> (Int, Int) -> Cell
newCell Nothing (x,y) = this Nothing
	where this _ = Cell {
		options = [1..9],
		value = Nothing,
		position = (x,y)
	}

newCell (Just val) (x,y) = this val
	where this val = Cell {
		options = [],
		value = Just val,
		position = (x,y)
	}
	
emptyCell pos = newCell Nothing pos

readCell :: Char -> (Int, Int) -> Cell
readCell '_' pos = emptyCell pos
readCell ch pos = newCell (Just (digitToInt ch)) pos

showCell :: Cell -> String
showCell cell = ' ':(maybe "_" show (cell /> value))

parseCells :: [Char] -> [[Cell]]
parseCells str = map (
		\(rowNum, val) ->
			[readCell (value!!0) (rowNum, colNum) |
				 (colNum, value) <- zip [0..] (splitOn " " val)]
		) (zip [0..] (lines str))
		
getRowNum :: Cell -> Int
getRowNum cell = fst (cell /> position)

getColNum :: Cell -> Int
getColNum cell = snd (cell /> position)

getGridNum :: Cell -> Int
getGridNum cell = 3 * (quot (getRowNum cell) 3) + (quot (getColNum cell) 3)


-- GRID
loadGrid :: [Char] -> Grid
loadGrid str = Grid {cells = parseCells str }

showGrid :: Grid -> [Char]
showGrid grid = unlines $ map (\row -> concat $ map showCell row) (grid /> cells)

getCellsInRow :: Grid -> Int -> [Cell]
getCellsInRow grid rowNum = (grid /> cells) !! rowNum

getCellsInCol :: Grid -> Int -> [Cell]
getCellsInCol grid colNum = map (\row -> row !! colNum) (grid /> cells)

getCellsInGrid :: Grid -> Int -> [Cell]
getCellsInGrid grid gridNum = do
	let rowStart = (quot gridNum 3) * 3
	let rowEnd = rowStart + 2
	let colStart = (mod gridNum 3) * 3
	let colEnd = colStart + 2
	concat $ map (\x -> subArr x [colStart..colEnd]) $ subArr (grid/>cells) [rowStart..rowEnd]


--SOLVER
getMayBeVal :: Maybe Int -> Int
getMayBeVal Nothing = -1
getMayBeVal (Just x) = x

remaining :: [Cell] -> [Int]
remaining cells = filter (/= -1) $ [getMayBeVal (value x) | x <- cells]

getLinkedCells :: Grid -> Cell -> [Cell]
getLinkedCells grid cell = nub $ ((getCellsInRow grid $ getRowNum cell) ++
							(getCellsInCol grid $ getColNum cell) ++ 
							(getCellsInGrid grid $ getGridNum cell))

getEmptyCells :: [Cell] -> [Cell]
getEmptyCells cells = filter (\x -> isNothing (value x)) cells

calcPossib :: Grid -> Cell -> Cell
calcPossib grid cell
	| -1 == getMayBeVal (value cell)  = cell {
		options = [1..9] \\ (nub $ remaining $ getLinkedCells grid cell) :: [Int]
		}
	| otherwise = cell

groupCellsByOptions :: Cell -> [Cell] -> [[Cell]]	
groupCellsByOptions cell cells = [
		[c | c <- getEmptyCells cells, elem opt (options c)] |
		opt <- [1..9]
	]

fixCell :: (Grid -> Int -> [Cell]) -> (Cell -> Int) -> Grid -> Cell -> Cell
fixCell fn numFn grid cell
	| -1 == getMayBeVal (value cell) = do
		let temp = groupCellsByOptions cell $ getEmptyCells $ fn grid $ numFn cell
		let filtered = filter (\(x, pos) -> (1 == (length x)) && ((x!!0) == cell)) (zip temp [0..])
		let ret = if length (filtered) > 0 then
					cell {
						options = {-trace ("Filtered: " ++ show filtered ++ "\ntemp: " ++ show temp)-} [],
						value = Just $ 1 + (snd $ filtered !! (trace ("Fixed cell:" ++ show cell) 0))
					}
				else
					cell
		ret
	| otherwise = cell

processCell :: Grid -> Cell -> Cell
processCell grid cell
	| -1 == getMayBeVal (value cell) = do
		let ret1 = fixCell getCellsInRow getRowNum grid cell
		let ret2 = fixCell getCellsInCol getColNum grid cell
		let ret3 = fixCell getCellsInGrid getGridNum grid cell
		ret3
	| otherwise = cell

solveGrid :: Grid -> Grid
solveGrid grid = do
	let grid' = grid {
		cells = [[calcPossib grid cell | cell <- row] | row <- grid /> cells]
	}
	grid' {
		cells = [[processCell grid' cell | cell <- row] | row <- cells grid']
	}
	

solveFully :: Grid -> Grid
solveFully grid = do
	let grid' = solveGrid grid
	if grid' == grid then
		grid
	else
		solveFully grid'


debugLinkedEmptyCells grid (x, y) = show $getEmptyCells $ getLinkedCells grid ((cells grid)!!x!!y)


debugCellOptionGroups grid (x, y) = do
	let cell = (grid /> cells)!!x !! y
	let emptyLinked = getEmptyCells $ getLinkedCells grid cell
	show $ groupCellsByOptions cell emptyLinked
	
main = do
	s <- readFile "input.txt"
	let grid = loadGrid s
	putStrLn $ showGrid grid
	putStrLn "______"
	putStrLn $ showGrid $ solveFully grid
	
	
	{-
	let grid' = grid {
			cells = [[calcPossib grid cell | cell <- row] | row <- grid /> cells]
	putStrLn "==========="
	
	putStrLn $ debugLinkedEmptyCells grid' (7,3)
	
	putStrLn "==========="
	
	putStrLn $ debugCellOptionGroups grid' (7,3)
		-}
