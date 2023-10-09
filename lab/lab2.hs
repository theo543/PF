-- 1

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x * x + b * x + c

-- 2

eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

-- 3

-- fibonacci with case
fbCase :: Integer -> String
fbCase x = let
  by3 :: Bool
  by3 = (x `mod` 3) == 0
  by5 :: Bool
  by5 = (x `mod` 5) == 0
  in
  case (by3, by5) of (True, True) -> "FizzBuzz"
                     (False, True) -> "Buzz"
                     (True, False) -> "Fizz"
                     (False, False) -> show x

-- fibonacci with if
fbIf :: Integer -> String
fbIf x = let
  by3 :: Bool
  by3 = (x `mod` 3) == 0
  by5 :: Bool
  by5 = (x `mod` 5) == 0
  in
  if by3 && by5 then "FizzBuzz"
  else if by3 then "Fizz"
  else if by5 then "Buzz"
  else show x

-- 4

-- tribonacci with patterns
tbPt :: Integer -> Integer
tbPt 1 = 1
tbPt 2 = 1
tbPt 3 = 2
tbPt x = tbPt (x - 1) + tbPt (x - 2) + tbPt (x - 3)

-- tribonacci with guards
tbGd :: (Eq t, Num t, Num a) => t -> a
tbGd x
    | x == 1 = 1
    | x == 2 = 1
    | x == 3 = 2
    | otherwise = tbGd (x - 1) + tbGd (x - 2) + tbGd (x - 3)

-- 5

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

-- 6

verifL :: [Int] -> Bool
verifL = even . length

takefinal :: [a] -> Int -> [a]
takefinal l n = drop (max 0 $ length l - n) l

remove :: [a] -> Int -> [a]
remove l n = take (n - 1) l ++ drop n l

-- 7

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n v = v : myreplicate (n - 1) v

oddOr0 :: Integral a => a -> a
oddOr0 n = if even n then 0 else n

sumImp :: Integral a => [a] -> a
sumImp [] = 0
sumImp (head:tail) = oddOr0 head + sumImp tail

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (('A':s):sl) = length s + 1 + totalLen sl
totalLen (_:sl) = totalLen sl
