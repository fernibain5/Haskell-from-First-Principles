
{-# LANGUAGE ParallelListComp #-}
module ChapterExercises where 



import Control.Monad

-- # 1

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg


-- # 2

data PhhhbbtttEither b a = Left' a | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' $ f x

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  (Right' b) <*> (Right' b') = Right' $ b <> b'
  (Right' b) <*> _ = Right' b
  _ <*> (Right' b) = Right' b
  (Left' f) <*> (Left' x) = Left' $ f x
  
instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  (Right' x) >>= _ = Right' x
  (Left' x) >>= f = f x

-- # 3

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x 

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

-- # 4


data List a =
  Nil
  | Cons a (List a)

-- instance Semigroup (List a) where
  

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

-- instance Applicative List where
--   pure x = Cons x Nil
--   (<*>) Nil Nil = Nil
--   (<*>) Nil x = x
--   (<*>) x Nil = x
--   (<*>) (Cons f lf) x = Cons (f x) 
  

-- instance Monad List where
--   return = pure
--   (>>=) (List a) f = f a

classify :: Int -> Int
classify n 
  | n > 0 = aliquotSum
  | otherwise = 0
  where aliquoutSum = [  x  | x <- [1..10] | y <- [1..23] ]