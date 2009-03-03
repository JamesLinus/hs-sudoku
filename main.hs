import List (nubBy, (\\))

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
                 where others = cycle ["", "", "\n- - - + - - - + - - -"]
                       strip  = unlines . init . lines . unlines

rowToString :: Row -> String
rowToString row = strip $ foldl (++) "" $ zipWith (++) (map (slotToString) row) others
                  where others = cycle [" ", " ", " | "]
                        strip  = init . init . init

slotToString :: Slot -> String
slotToString Nothing  = "_"
slotToString (Just x) = show x

-- We assume no duplicate numbers in a row
rowIsValid :: Row -> Bool
rowIsValid row = (length $ withoutBlanks row) == (length row)

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


