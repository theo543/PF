myInt = 31415926535897932384626433832795028841971693993751058209749445923
double :: Integer -> Integer
double x = x+x
triple :: Integer -> Integer
triple x = x+x+x
maxim :: Integer -> Integer -> Integer
maxim x y =
    if (x > y)
        then x
        else y
maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z =
    maxim x (maxim y z)

sumsqrt :: Num a => a -> a -> a
sumsqrt x y = x * x + y * y

parity :: Integral a => a -> String
parity x = if even x then "par" else "impar"

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n - 1)

is1gtthan2times2 :: (Ord a, Num a) => a -> a -> Bool
is1gtthan2times2 x y = x > y * 2

maxElem :: Ord a => [a] -> a
maxElem [single] = single
maxElem (head:tail) = max head $ maxElem tail
