-- 1.

sumSquareOdd :: [Int] -> Int
sumSquareOdd = foldr (\x y -> x^2 + y) 0 . filterFoldr odd

-- 2.

myand :: [Bool] -> Bool
myand = foldr (&&) True

myor :: [Bool] -> Bool
myor = foldr (||) False

-- 3.

allVerifies :: (a -> Bool) -> [a] -> Bool
allVerifies p l = myand $ mapFoldr p l

-- 4.

anyVerifies :: (a -> Bool) -> [a] -> Bool
anyVerifies p l = myor $ mapFoldr p l

-- 5.

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\a b -> f a : b) []

filterFoldr' :: (a -> Bool) -> a -> [a] -> [a]
filterFoldr' f a b | f a = a : b
filterFoldr' f a b = b

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (filterFoldr' f) []

-- 6.

listToInt :: [Integer] -> Integer
listToInt = foldl (\a b -> a * 10 + b) 0

-- 7.

rmChar :: Char -> String -> String
rmChar c = filterFoldr (/= c)

rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs $ rmChar x str

rmCharsFold :: String -> String -> String
rmCharsFold rm str = foldr rmChar str rm

-- 8.

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 9.

myElem :: Eq a => a -> [a] -> Bool
myElem elem = myor . mapFoldr (== elem)

-- 10.

myUnzip' :: (a, b) -> ([a], [b]) -> ([a], [b])
myUnzip' (a, b) (ax, bx) = (a : ax, b : bx)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr myUnzip' ([], [])
