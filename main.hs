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
