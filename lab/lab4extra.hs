data Cell = X | O | Blank deriving (Eq, Show)
type Board = [[Cell]]

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

enumerate2d :: Board -> [[(Int, Int, Cell)]]
enumerate2d = zipWith (\x -> zipWith (\y elem -> (x, y, elem)) [1 .. ]) [1..]

stepAt :: Cell -> Int -> Int -> Board -> Board
stepAt c x y l = map2d (\(x', y', elem) -> if x == x' && y == y' && elem == Blank then c else elem) $ enumerate2d l

numOf' :: Cell -> [Cell] -> Int
numOf' c = sum . map (\c' -> if c == c' then 1 else 0)

numOf :: Cell -> Board -> Int
numOf c = sum . map (numOf' c)

step :: Cell -> Board -> [Board]
step c b = let
    len_outer = length b
    len_inner = length (head b)
    iter = if len_outer == len_inner then [1..len_outer] else error "non-square"
    lists = [stepAt c x y b | x <- iter, y <- iter]
    in
    filter (\b' -> numOf c b' /= numOf c b) lists


next :: Cell -> [Board] -> [Board]
next c = concatMap (step c)

lineWins :: Cell -> [Cell] -> Bool
lineWins c = all (== c)

hasVertical' :: Cell -> Board -> Int -> Bool
hasVertical' c b x = let
    v = map (!! (x - 1)) b
    in
    lineWins c v

hasVertical :: Cell -> Board -> Bool
hasVertical c b = any (hasVertical' c b) [1..length b]

hasHorizontal' :: Cell -> Board -> Int -> Bool
hasHorizontal' c b y = lineWins c (b !! (y - 1))

hasHorizontal :: Cell -> Board -> Bool
hasHorizontal c b = any (hasHorizontal' c b) [1..length b]

topLeftDiagonal :: Board -> [Cell]
topLeftDiagonal b = map (\(_, _, elem) -> elem) $ filter (\(x, y, _) -> x == y) (concat $ enumerate2d b)

topRightDiagonal :: Board -> [Cell]
topRightDiagonal b = map (\(_, _, elem) -> elem) $ filter (\(x, y, _) -> x == (length b - y + 1)) (concat $ enumerate2d b)

boardWins :: Cell -> Board -> Bool
boardWins c b = hasHorizontal c b || hasVertical c b || lineWins c (topLeftDiagonal b) || lineWins c (topRightDiagonal b)

win :: Cell -> Board -> [Board]
win c b = filter (boardWins c) $ step c b
