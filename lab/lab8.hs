class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys = map fst . toList
  values :: c key value -> [value]
  values = map snd . toList
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  fromList = foldr (uncurry insert) empty

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty :: PairList key value
    empty = PairList []
    singleton :: key -> value -> PairList key value
    singleton key val = PairList [(key, val)]
    insert :: Ord key => key -> value -> PairList key value -> PairList key value
    insert key val = PairList . ((key, val) : ) . getPairList
    clookup :: Ord key => key -> PairList key value -> Maybe value
    clookup key (PairList list) = case filter ((== key) . fst) list of
        (_, val):_ -> Just val
        [] -> Nothing
    delete :: Ord key => key -> PairList key value -> PairList key value
    delete key = PairList . filter ((/= key) . fst) . getPairList
    toList :: PairList key value -> [(key, value)]
    toList = getPairList

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty :: SearchTree key value
    empty = Empty
    singleton :: key -> value -> SearchTree key value
    singleton key val = BNode Empty key (Just val) Empty
    insert :: Ord key => key -> value -> SearchTree key value -> SearchTree key value
    insert key val Empty = singleton key val
    insert key val (BNode l key' _ r) | key == key' = BNode l key (Just val) r
    insert key val (BNode l key' val' r) | key < key' = BNode (insert key val l) key' val' r
    insert key val (BNode l key' val' r) | key > key' = BNode l key' val' (insert key val r)
    clookup :: Ord key => key -> SearchTree key value -> Maybe value
    clookup _ Empty = Nothing
    clookup key (BNode _ key' val _) | key == key' = val
    clookup key (BNode l key' _ _) | key < key' = clookup key l
    clookup key (BNode _ key' _ r) | key > key' = clookup key r
    delete :: Ord key => key -> SearchTree key value -> SearchTree key value
    delete _ Empty = Empty
    delete key (BNode l key' _ r) | key == key' = BNode l key' Nothing r
    delete key (BNode l key' val' r) | key < key' = BNode (delete key l) key' val' r
    delete key (BNode l key' val' r) | key > key' = BNode l key' val' (delete key r)
    toList :: SearchTree key value -> [(key, value)]
    toList Empty = []
    toList (BNode l key Nothing r) = toList l ++ toList r
    toList (BNode l key (Just val) r) = toList l ++ (key, val) : toList r

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
            toArb :: a -> Arb
            fromArb :: Arb -> a
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

