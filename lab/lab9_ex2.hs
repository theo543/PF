{-# Language DefaultSignatures #-}

-- 2. Instant, iat, i clasa Scalar folosindu-v ̆a de tipuri primitive (hint: nu uitat, i, trebuie s ̆a fie corpuri
-- comutative). Apoi, considerat, i clasa de mai jos a vectorilor.

class Scalar a where
  zero :: a
  default zero :: (Num a) => a
  zero = 0

  one :: a
  default one :: (Num a) => a
  one = 1

  adds :: a -> a -> a
  default adds :: (Num a) => a -> a -> a
  adds = (+)

  mult :: a -> a -> a
  default mult :: (Num a) => a -> a -> a
  mult = (*)

  negates :: a -> a
  default negates :: (Num a) => a -> a
  negates = (0 -)

  recips :: a -> a
  default recips :: (Num a, Fractional a) => a -> a
  recips = (1 /)

instance Scalar Int where
    recips :: Int -> Int
    recips = (1 `div`)
instance Scalar Integer where
    recips :: Integer -> Integer
    recips = (1 `div`)
instance Scalar Float
instance Scalar Double

-- 3. Scriet, i dou ̆a instant, e ale clasei Vector pentru a reprezenta vectori bidimensionali s, i tridimen-
-- sionali.

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

