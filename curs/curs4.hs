import Data.Char (ord, isDigit)

squares :: [Int] -> [Int]
squares = map (^ 2)

ords :: [Char] -> [Int]
ords = map ord

positives :: [Int] -> [Int]
positives = filter (> 0)

digits :: [Char] -> [Char]
digits = filter isDigit

data NamedPredicate a = NP {name :: String, predicate :: a -> Bool}

instance Show (NamedPredicate a) where
    show :: NamedPredicate a -> String
    show p = "function named '" ++ name p ++ "'"

namedFns :: [NamedPredicate Int]
namedFns = filter (($ 5) . predicate) [NP "> 10" (> 10), NP "> 2" (> 2), NP "> 4" (> 4), NP "is odd" odd, NP "is even" even, NP "is not 0" (/= 0)]
