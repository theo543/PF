($>) :: a -> (a -> b) -> b
($>) a f = f a
infixl 0 $>

main = [1, 2, 3, 4, 5] $> filter even $> map (+ 10) $> print
