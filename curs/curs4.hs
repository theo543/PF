import Data.Char (ord, isDigit)

squares :: [Int] -> [Int]
squares = map (^ 2)

ords :: [Char] -> [Int]
ords = map ord

positives :: [Int] -> [Int]
positives = filter (> 0)

digits :: [Char] -> [Char]
digits = filter isDigit

filteredfns = filter (\(name, fn) -> fn 5) [("> 10", (> 10)), ("> 2", (> 2)), ("> 4", (> 4)), ("is odd", odd), ("is even", even), ("is not 0", (/= 0))]
showfns = map (\(name, fn) -> "5 " ++ name) filteredfns
