import Data.List (nub, sortOn)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Control.Monad (liftM2)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

varP = Var "P"
notP = Not varP
varQ = Var "Q"
notQ = Not varQ
varR = Var "R"
notR = Not varR

p1 :: Prop
p1 = (varP :|: varQ) :&: (varP :&: varQ)

p2 :: Prop
p2 = (varP :|: varQ) :&: (notP :&: notQ)

p3 :: Prop
p3 = (varP :&: (varQ :|: varR)) :&: ((notP :|: notQ) :&: (notP :|: notR))

instance Show Prop where
  show :: Prop -> String
  show (Var str) = str
  show F = "False"
  show T = "True"
  show (Not p) = printf "(~%s)" (show p)
  show (p1 :|: p2) = printf "(%s|%s)" (show p1) (show p2)
  show (p1 :&: p2) = printf "(%s&%s)" (show p1) (show p2)

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

eval :: Prop -> Env -> Maybe Bool
eval (Var v) env = lookup v env
eval T _ = Just True
eval F _ = Just False
eval (Not p) env = fmap not (eval p env)
eval (p1 :|: p2) env = liftM2 (||) (eval p1 env) (eval p2 env)
eval (p1 :&: p2) env = liftM2 (&&) (eval p1 env) (eval p2 env)

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == Just True

variabile :: Prop -> [Nume]
variabile = nub . variabile'
  where
    variabile' :: Prop -> [Nume]
    variabile' (Var v) = [v]
    variabile' (Not p) = variabile' p
    variabile' T = []
    variabile' F = []
    variabile' (p1 :|: p2) = variabile' p1 ++ variabile' p2
    variabile' (p1 :&: p2) = variabile' p1 ++ variabile' p2

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = sortOn (map snd) unsorted
  where
    unsorted = do
      prev :: Env <- envs xs
      [(x, False) : prev, (x, True) : prev]

test_envs =
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

satisfiabila :: Prop -> Bool
satisfiabila prop = let
    possible_envs = envs $ variabile prop
    possible_evals = map ((== Just True) . eval prop) possible_envs
  in
    or possible_evals

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida prop = not $ satisfiabila (Not prop)

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True


echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = not $ satisfiabila (p1 :&: Not p2)

test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))

test_all = and tests
  where tests = [test_ShowProp, test_echivalenta2, test_envs, test_satisfiabila1,
                 test_valida1, test_variabile, test_echivalenta1, test_echivalenta3,
                 test_eval, test_satisfiabila2, test_valida2]
