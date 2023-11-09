data ForeverList a = ForeverCons a (ForeverList a)
    deriving Show

foreverList :: Int -> ForeverList Int
foreverList nr = let x = ForeverCons nr x in x

addints :: [Either Int String] -> Int
addints = foldr addints' 0
    where
        addints' :: Either Int String -> Int -> Int
        addints' (Left x) y = x + y
        addints' (Right _) y = y

addstrs :: [Either Int String] -> String
addstrs = foldr addstrs' ""
    where
        addstrs' :: Either Int String -> String -> String
        addstrs' (Left _) y = y
        addstrs' (Right x) y = x ++ y
