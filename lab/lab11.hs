newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

newtype Constant a b = Constant b
instance Functor (Constant c) where
    fmap f (Constant x) = Constant (f x)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b x) = Three a b (f x)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a x1 x2) = Three' a (f x1) (f x2)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c x) = Four a b c (f x)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 x) = Four'' a1 a2 a3 (f x)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor x) = Bloor (f x)

newtype LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap fn (LiftItOut x) = LiftItOut (fmap fn x)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap fn (DaWrappa x y) = DaWrappa (fmap fn x) (fmap fn y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap fn (IgnoringSomething f_a x) = IgnoringSomething f_a (fmap fn x)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap fn (Notorious g_o g_a x) = Notorious g_o g_a (fmap fn x)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str x) = Print str (f x)
    fmap f (Read x) = Read (f . x)
