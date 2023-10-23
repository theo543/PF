factori :: Int -> [Int]
factori x = [div | div <- [1..abs x], x `rem` div == 0]

prim :: Int -> Bool
prim x = factori x == [1, abs x]

numerePrime :: Int -> [Int]
numerePrime n | n < 0 = map (0 -) $ numerePrime (abs n)
numerePrime n = [p | p <- [2..n], prim p]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 a b c = zipWith (\a (b, c) -> (a, b, c)) a (zip b c)

firstEl :: [(a, b)] -> [a]
firstEl = map fst

sumList :: [[Int]] -> [Int]
sumList = map sum

prel2' :: Int -> Int
prel2' x | even x = x `div` 2
prel2' x = x * 2
prel2 :: [Int] -> [Int]
prel2 = map prel2'

filterHasChar :: Char -> [String] -> [String]
filterHasChar c = filter (elem c)

squareOdds :: [Int] -> [Int]
squareOdds = map (^ 2) . filter odd

squareOddPos :: [Int] -> [Int]
squareOddPos = map (\(pos, elem) -> elem ^ 2) . filter (\(pos, _) -> odd pos) . zip [1..]

numaiVocale :: [String] -> [String]
numaiVocale = map $ filter (`elem` "aeiou")

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) | f x = x : myfilter f xs
myfilter f (_:xs) = myfilter f xs
