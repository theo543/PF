import Control.Monad (liftM2)
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.Reader (Reader, reader, runReader)

{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

{-
a) Definiți addM prin orice metodă (de exemplu, folosind șabloane).
-}

fct :: Maybe Int ->  Maybe Bool
fct x = do
    nr <- x
    return $ pos nr

{-
2. Vrem să definim o funcție care adună două valori de tip Maybe Int:

a) Definiți addM prin orice metodă (de exemplu, folosind șabloane).
b) Definiți addM folosind operații monadice și notația do.
-}

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM = liftM2 (+)

addM_do :: Maybe Int -> Maybe Int -> Maybe Int
addM_do xM yM = do
  x <- xM
  y <- yM
  return $ x + y

-- 3. Rescrieți următoarele funcții folosind notația do:

--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- prod f xs ys = [f x y | x <- xs, y<-ys]
prod f xs ys = do
  x <- xs
  y <- ys
  return $ f x y

myGetLine :: IO String
{-
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)
-}
myGetLine = do
  x <- getChar
  if x == '\n' then
    return []
  else do
    xs <- myGetLine
    return (x:xs)

-- 4. Rescrieți următoarea funcție folosind notația cu secvențiere:

prelNo noin =  sqrt noin

{-
ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout
-}

ioNumber = (readLn :: IO Float) >>= \noin ->
          putStrLn ("Intrare\n" ++ (show noin)) >>
          let noout = prelNo noin in
          putStrLn "Iesire" >>
          print noout

{-
5.
a) Definiți funcțiile logIncrement și logIncrement2 din curs și testați-le.
b) Definiți funcția logIncrementN, care generalizeaz a logIncrement2, astfel:
-}

fmtIncrement :: Int -> String
fmtIncrement x = "increment: " ++ show x ++ "\n"

logIncrement :: Int -> Writer String Int
logIncrement x = do
  tell $ fmtIncrement x
  return $ x + 1

logIncrementN :: Int -> Int -> Writer String Int
logIncrementN 0 x = return x
logIncrementN times x = logIncrement x >>= logIncrementN (times - 1)

logIncrement2 :: Int -> Writer String Int
logIncrement2 = logIncrementN 2

logIncrementL :: Int -> Writer [String] Int
logIncrementL x = do
  tell [fmtIncrement x]
  return $ x + 1

logIncrementNL :: Int -> Int -> Writer [String] Int
logIncrementNL 0 x = return x
logIncrementNL times x = logIncrementL x >>= logIncrementNL (times - 1)

-- 6.

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = "NAME:" ++ name p
showPersonA :: Person -> String
showPersonA p = "AGE: " ++ show (age p)

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

mshowPersonN :: Reader Person String
mshowPersonN = fmap ("NAME: " ++) $ reader name

mshowPersonA ::  Reader Person String
mshowPersonA = fmap ("AGE: " ++) $ reader $ show . age

mshowPerson ::  Reader Person String
mshowPerson = do
  n <- mshowPersonN
  a <- mshowPersonA
  return $ "(" ++ n ++ ", " ++ a ++ ")"

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}
