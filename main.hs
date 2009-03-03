import List (nubBy)

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

rowIsValid :: Row -> Bool
rowIsValid row = (length rowWithoutNothing) == (length rowWithoutDups)
                 where rowWithoutNothing = filter (/= Nothing) row
                       rowWithoutDups    = List.nubBy (==) rowWithoutNothing

columnToRow :: Int -> Board -> Row
columnToRow i = map (!! i)

squareToRow :: Int -> Int -> Board -> Row
squareToRow 0 0 board = flatten $ take 3 $ map (take 3) board
squareToRow x 0 board = squareToRow (x-1) 0  $ map (tail . tail . tail) board
squareToRow x y board = squareToRow x (y-1) $ (tail . tail . tail) board

flatten :: [[a]] -> [a]
flatten []     = []
flatten (x:xs) = x ++ (flatten xs)

