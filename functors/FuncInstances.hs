-- FuncInstances

module FuncInstances where



newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity z) = Identity $ f z

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair z z') = Pair (f z) (f z')

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two z z') = Two z $ f z'

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three z z' z'') = Three z z' (f z'')

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' z z' z'') = Three' z (f z') (f z'')

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
 fmap f (Four' a b c d) = Four' a b c $ f d





data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second z) = Second $ f z
  fmap _ (First z) = First z 

data Quant a b = Finance | Desk a | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where 
  fmap f (Bloor a) = Bloor $ f a
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

data K a b = K a
           deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

data LiftItOut f a = LiftItOut (f a)
                   deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap func (LiftItOut x) = LiftItOut (fmap func x)

data Parappa f g a = DaWrappa (f a) (g a)
                   deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
                       deriving (Eq, Show)
 
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)


data Notorious g o a t = Notorious (g o) (g a) (g t)
                       deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

data List a = Nil | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap f (Cons a l) = Cons (f a) $ fmap f l
  fmap _ Nil               = Nil            

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap f (MoreGoats gl gl' gl'') = MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')
  fmap f (OneGoat a) = OneGoat $ f a
  fmap _ NoGoat = NoGoat

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
 fmap _ Halt = Halt
 fmap f (Print s a) = Print s $ f a
 fmap f (Read sa) = Read $ fmap f sa