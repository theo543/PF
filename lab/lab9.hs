data Tree = Empty  -- arbore vid
   | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina si 3 fii
    deriving Show

extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty)
                (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

-- 1. Instant, iat, i clasa urm ̆atoare pentru tipul Tree.

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; pt un arbore vid
                      -- se considera ca are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

sumWith :: Num c => (a -> c) -> [a] -> c
sumWith f = sum . map f

maximumWith :: Ord c => (a -> c) -> [a] -> c
maximumWith f = maximum . map f

instance ArbInfo Tree where
    level :: Tree -> Int
    level Empty = 0
    level (Node _ a1 a2 a3) = 1 + maximumWith level [a1, a2, a3]

    sumval :: Tree -> Int
    sumval Empty = 0
    sumval (Node val a1 a2 a3) = val + sumWith sumval [a1, a2, a3]

    nrFrunze :: Tree -> Int
    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ a1 a2 a3) = sumWith nrFrunze [a1, a2, a3]

test_level :: Bool
test_level = level extree == 3

test_sumval :: Bool
test_sumval = sumval extree == 13

test_nrFrunze :: Bool
test_nrFrunze = nrFrunze extree == 2

class Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

