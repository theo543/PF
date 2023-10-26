import Data.Char (ord)

squares :: [Int] -> [Int]
squares = map (^ 2)

ords :: [Char] -> [Int]
ords = map ord
