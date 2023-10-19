import Data.Char (isDigit, digitToInt)
import Debug.Trace (trace)

palindrome :: Eq a => [a] -> Bool
palindrome l = l == reverse l

vowelsInStr :: [Char] -> Int
vowelsInStr = length . filter (`elem` "aeiou")

nrVocale :: [[Char]] -> Int
nrVocale = sum . map vowelsInStr . filter palindrome

afterEven :: Int -> Int -> [Int]
afterEven toinsert elem
        | even elem = [elem, toinsert]
        | otherwise = [elem]

afterEvens :: Int -> [Int] -> [Int]
afterEvens toinsert = (>>= afterEven toinsert)

divizori :: Int -> [Int]
divizori x = filter ((== 0) . rem x) [1..abs x]

divizoriComp :: Int -> [Int]
divizoriComp x = [d | d <- [1..x], x `rem` d == 0]

listaDiv :: [Int] -> [[Int]]
listaDiv = map divizori

type IntervalFilter = Int -> Int -> [Int] -> [Int]

inInterval :: IntervalFilter
inInterval low high = filter (\x -> x >= low && x <= high)

inIntervalRec :: IntervalFilter
inIntervalRec _ _ [] = []
inIntervalRec low high (x:xs)
                        | x >= low && x <= high = x:inIntervalRec low high xs
                        | otherwise = inIntervalRec low high xs

inIntervalComp :: IntervalFilter
inIntervalComp low high list = [x | x <- list, x >= low && x <= high]

pozitive :: [Int] -> Int
pozitive = length . filter (> 0)

btoi :: Bool -> Int
btoi True = 1
btoi False = 0

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = btoi (x > 0) + pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp l = sum [1 | x <- l, x > 0]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare = map fst . filter (odd . snd) . enumerate

pozitiiImpareRec' :: [(Int, Int)] -> [Int]
pozitiiImpareRec' [] = []
pozitiiImpareRec' ((pos,elem):xs)
            | odd elem = pos:pozitiiImpareRec' xs
            | otherwise = pozitiiImpareRec' xs

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec = pozitiiImpareRec' . enumerate

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [pos | (pos, elem) <- enumerate l, odd elem]

multDigits :: [Char] -> Int
multDigits = product . map digitToInt . filter isDigit

multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
                  | isDigit x = digitToInt x * multDigitsRec xs
                  | otherwise = multDigitsRec xs

multDigitsComp :: [Char] -> Int
multDigitsComp l = product [digitToInt x | x <- l, isDigit x]

{-
permutations' :: [a] -> a -> [a] -> [[a]]
permutations' xp x xn = (let p' = permutations (xp++xn) in map (x :) p') ++ permutations'' xp x xn
permutations'' :: [a] -> a -> [a] -> [[a]]
permutations'' _ _ [] = []
permutations'' p x (n:nx) = permutations' (p++[x]) n nx
permutations :: [a] -> [[a]]
permutations [] = []
permutations (x:xs) = permutations' [] x xs 
-}

permutations' :: (a, [a]) -> [[a]]
permutations' (elem, sublist) = let subperms = permutations sublist in
                           map (elem :) subperms

permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
-- for each index, find the permutations which start with that element using helper function, concat the [[[a]]] into [[a]]
permutations l = let splits = [(l !! (x - 1), take (x-1) l ++ drop x l) | x <- [1..length l]] in
                 concatMap permutations' splits

-- for testing speed
main = (readLn :: IO Int) >>= (\len -> print . show . length . permutations $ [1..len])
