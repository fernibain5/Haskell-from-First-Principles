module ChapterExercises21 where


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a



newtype Constant' a b =
  Constant' { getConstant' :: a }

instance Functor (Constant' a) where 
  fmap f (Constant' a) = Constant' a

instance Foldable (Constant' a) where
  foldMap _ _ = mempty

instance Traversable (Constant' a) where
  traverse f (Constant' a) = pure (Constant' a)


data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where 
  fmap f (Yep a) = Yep $ f a
  fmap _ Nada    = Nada

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ _       = mempty

instance Traversable Optional where
  traverse f Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where 
  fmap f (Cons a la) = Cons (f a) (fmap f la)
  fmap _ Nil    = Nil

instance Foldable List where
  foldMap f (Cons a la) = f a <> foldMap f la
  foldMap _ Nil       = mempty

instance Traversable List where
  traverse f Nil    = pure Nil
  traverse f (Cons a la) = Cons <$> f a <*> traverse f la


data Three' a b c = Three' a b c
  deriving (Eq, Show)

instance Functor (Three' a b) where 
  fmap f (Three' a b c) = Three' a b $ f c

instance Foldable (Three' a b) where
  foldMap f (Three' _ _ c) = f c

instance Traversable (Three' a b) where
  traverse f (Three' a b c) = Three' a b <$> f c



data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where 
  fmap f (Pair a c) = Pair a $ f c

instance Foldable (Pair a) where
  foldMap f (Pair _ c) = f c

instance Traversable (Pair a) where
  traverse f (Pair a c) = Pair a <$> f c



data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where 
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'



data Bigger a b = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where 
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b'' 



data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = f a <> foldMap f n

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a



data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2




