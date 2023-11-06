data Fruct
  = Mar String Bool
  | Portocala String Int
    deriving Show

ionatanFaraVierme :: Fruct
ionatanFaraVierme = Mar "Ionatan" False

goldenCuVierme :: Fruct
goldenCuVierme = Mar "Golden Delicious" True

portocalaSicilia10 :: Fruct
portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe :: [Fruct]
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala tip _) = tip `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 :: Bool
test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True

test_ePortocalaDeSicilia2 :: Bool
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia' :: Fruct -> Int
nrFeliiSicilia' fr@(Portocala tip nr) | ePortocalaDeSicilia fr = nr
nrFeliiSicilia' _ = 0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = sum . map nrFeliiSicilia'

test_nrFeliiSicilia :: Bool
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi' :: Fruct -> Bool
nrMereViermi' (Mar _ arevierme) = arevierme
nrMereViermi' _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi = length . filter nrMereViermi'

test_nrMereViermi :: Bool
test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa _ = Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

sumL :: Linie -> Int
sumL (L l) = sum l

verifica :: Matrice -> Int -> Bool
verifica (M l) nr = all ((nr ==) . sumL) l

test_verif1 :: Bool
test_verif1 = verifica (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 10 == False

test_verif2 :: Bool
test_verif2 = verifica (M [L [2,20,3], L [4,21], L [2,3,6,8,6], L [8,5,3,9]]) 25 == True

doarPozN' :: Int -> Linie -> Bool
doarPozN' n (L l) = length l /= n || all (> 0) l

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = all (doarPozN' n) m

testPoz1 :: Bool
testPoz1 = doarPozN (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == True

testPoz2 :: Bool
testPoz2 = doarPozN (M [L [1,2,-3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == False

corect' :: Int -> Linie -> Bool
corect' n (L l) = length l == n

corect :: Matrice -> Bool
corect (M []) = True
corect (M (L x:xs)) = all (corect' $ length x) xs

testcorect1 :: Bool
testcorect1 = corect (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) == False

testcorect2 :: Bool
testcorect2 = corect (M [L [1,2,3], L [4,5,8], L [3,6,8], L [8,5,3]]) == True
