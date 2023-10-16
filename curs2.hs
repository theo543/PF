anyfact :: (Num a, Ord a) => a -> a
anyfact 0 = 1
anyfact n | n > 0 = n * anyfact (n - 1)
anyfact n | n < 0 = - anyfact (- n)

posfact :: (Num a, Ord a) => a -> Maybe a
posfact n
       | n == 0 = Just 1
       | n > 0  = posfact (n - 1) >>= (Just . (* n))
       | otherwise = Nothing

posfact2 :: (Num t, Ord t) => t -> t
posfact2 0 = 1
posfact2 n | n > 0 = n * posfact2 (n - 1)
posfact2 n = error "n < 0"


main = readLn >>= (print . posfact) >> main
