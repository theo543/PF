data ForeverList a = ForeverCons a (ForeverList a)
    deriving Show

foreverList :: Int -> ForeverList Int
foreverList nr = let x = ForeverCons nr x in x
