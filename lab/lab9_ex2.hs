{-# Language DefaultSignatures #-}

-- 2. Instanțiază clasa Scalar folosindu-vă de tipuri primitive (hint: nu uitat, i, trebuie s ̆a fie corpuri
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

instance Scalar Float
instance Scalar Double

-- 3. Scrieți două instanțe ale clasei Vector pentru a reprezenta vectori bidimensionali s, i tridimen-
-- sionali.

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

data Vector2D a = Vector2D a a
    deriving (Show, Functor)

instance (Scalar a) => Vector Vector2D a where
    zerov = Vector2D zero zero
    onev = Vector2D one one
    addv (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 `adds` x2) (y1 `adds` y2)
    smult s = fmap (s `mult`)
    negatev = fmap negates

data Vector3D a = Vector3D a a a
    deriving (Show, Functor)

instance (Scalar a) => Vector Vector3D a where
    zerov = Vector3D zero zero zero
    onev = Vector3D one one one
    addv (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 `adds` x2) (y1 `adds` y2) (z1 `adds` z2)
    smult s = fmap (s `mult`)
    negatev = fmap negates
