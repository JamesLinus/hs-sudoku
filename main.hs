import List (find, (\\), nubBy)

type Slot  = Maybe Int
type Row   = [Slot]
type Board = [Row]


-- Board Format
-- 0 9 8 | _ 9 1
-- 7 2 3 | 3 _ _
-- 1 6 5 |
-- - - - + - - - + - - -
-- etc.

emptyBoard :: Board
emptyBoard = [[Nothing | x <- [1..9]] | x <- [1..9]]

badBoard :: Board
badBoard = [[Just (x + y) | x <- [1..9]] | y <- [8,7..0]]

goodBoard :: Board
goodBoard = parseBoard "7 9 _ | _ _ _ | 3 _ _\n_ _ _ | _ _ 6 | 9 _ _\n8 _ _ | _ 3 _ | _ 7 6\n------+-------+------\n_ _ _ | _ _ 5 | _ _ 2\n_ _ 5 | 4 1 8 | 7 _ _\n4 _ _ | 7 _ _ | _ _ _\n------+-------+------\n6 1 _ | _ 9 _ | _ _ 8\n_ _ 2 | 3 _ _ | _ _ _\n_ _ 9 | _ _ _ | _ 5 4"



parseBoard :: String -> Board
parseBoard = map (parseRow) . filter ((/= '-') . head) . lines

parseRow :: String -> Row
parseRow = map (parseSlot) . filter (/= "|") . words

parseSlot :: String -> Slot
parseSlot "_" = Nothing
parseSlot str = Just (read str :: Int)

-- Refactor using 'intersperse'
boardToString :: Board -> String
boardToString board = strip $ zipWith (++) (map (rowToString) board) others
                 where others = cycle ["", "", "\n------+-------+------"]
                       strip  = unlines . init . lines . unlines

rowToString :: Row -> String
rowToString row = strip $ foldl (++) "" $ zipWith (++) (map (slotToString) row) others
                  where others = cycle [" ", " ", " | "]
                        strip  = init . init . init

slotToString :: Slot -> String
slotToString Nothing  = "_"
slotToString (Just x) = show x

-- To solve, can we make a pass over the board and collect all of the possible values in a spot. Many spots will only allow one element, at which point we change the board

-- Officially cant think anymore. Too late. Going to sleep.
isSolved :: Board -> Bool
isSolved board = and $ map (rowIsValid . (!!) board) [0..8]

hasBlank :: Board -> Bool
hasBlank = any (== Nothing) . flatten

-- We assume no duplicate numbers in a row
rowIsValid :: Row -> Bool
rowIsValid row = (length rowWithoutNothing) == (length rowWithoutDups)
                 where rowWithoutNothing = filter (/= Nothing) row
                       rowWithoutDups    = List.nubBy (==) rowWithoutNothing


withoutBlanks :: Row -> Row
withoutBlanks = filter (/= Nothing)

-- Could also be done using '(!! i) . transpose'
columnToRow :: Int -> Board -> Row
columnToRow i = map (!! i)

squareToRow :: Int -> Int -> Board -> Row
squareToRow 0 0 = flatten . take 3 . map (take 3)
squareToRow x 0 = squareToRow (x-1) 0 . map (tail . tail . tail)
squareToRow x y = squareToRow x (y-1) . (tail . tail . tail)

flatten :: [[a]] -> [a]
flatten []     = []
flatten (x:xs) = x ++ (flatten xs)

slotAt x y board = board !! y !! x

rowAt :: Int -> Int -> Board -> Row
rowAt x y = (!! y)

columnAt :: Int -> Int -> Board -> Row
columnAt x y = columnToRow x

squareAt :: Int -> Int -> Board -> Row
squareAt x y = squareToRow (x `div` 3) (y `div` 3)

possibleValuesFor :: Int -> Int -> Board -> Row
possibleValuesFor x y board = (map (Just) [1..9]) List.\\ usedValues
                              where usedValues = withoutBlanks $ flatten $ map (\f -> f x y board) [(rowAt), (columnAt), (squareAt)]

blankSlots :: Board -> [(Int, Int)]
blankSlots board = filter (slotIsBlank) coords
                   where coords = [(x,y) | y <- [0..8], x <- [0..8]]
                         slotIsBlank (x, y) = slotAt x y board == Nothing

allPossibleValuesFor :: Board -> [((Int, Int), Row)]
allPossibleValuesFor board = map (\(x,y) -> ((x,y), possibleValuesFor x y board)) $  blankSlots board 

placesWithOnePossibleValue :: Board -> [((Int, Int), Slot)]
placesWithOnePossibleValue = map (\(coords, (x:xs)) -> (coords, x)) . filter (\(_, possibilities) -> length possibilities == 1) . allPossibleValuesFor

makeMoves :: [((Int, Int), Slot)] -> Board -> Board
makeMoves [] board = if newMoves == [] then board else makeMoves newMoves board
                     where newMoves = placesWithOnePossibleValue board
makeMoves (((x,y), num):xs) board = makeMoves (xs ++ placesWithOnePossibleValue newBoard) newBoard
                                    where newBoard = (insertInMatrix x y num board)

insertAt x i xs = ys ++ [x] ++ zs
                  where (ys,z:zs) = splitAt i xs

insertInMatrix x y newValue board = insertAt (insertAt newValue x (board !! y)) y board




-- Finds all the places that this value could go in the specified block=
possiblePlacesFor :: Int -> Int -> Slot -> Board -> [((Int, Int), Row)]
possiblePlacesFor x y value board =























