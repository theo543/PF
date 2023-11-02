compose :: [b -> b] -> b -> b
compose = foldr (.) id

oneplussquare = compose [(+ 1), (^ 2)]

foldrNoAcc :: (b -> b -> b) -> [b] -> b
foldrNoAcc _ [] = error "empty list"
foldrNoAcc _ [a] = a
foldrNoAcc f (x:xs) = f x (foldrNoAcc f xs)
