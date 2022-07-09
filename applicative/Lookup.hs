-- Lookup.hs
module Lookup where

import Control.Applicative

import Data.List (elemIndex)


f x =
  lookup x [ (3, "hello")
  , (4, "julie")
  , (5, "kbai")]

g y =
  lookup y [ (7, "sup?")
  , (8, "chris")
  , (9, "aloha")]

h z =
  lookup z [(2, 3), (5, 6), (7, 8)]

m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

-- Exercises

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled =  (,) <$> y <*> z


x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int 
maxed = fmap max' x <*> y'


as = [1, 2, 3]

bs = [4, 5, 6]

a :: Maybe Integer
a = lookup 3 $ zip as bs

b :: Maybe Integer
b = lookup 2 $ zip as bs

summed :: Maybe Integer
summed = fmap sum $ (,) <$> a <*> b


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity ab) (Identity a) = Identity $ ab a

newtype Constant a b = 
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant {getConstant = a}

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant $ a <> b


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> Nil = Nil
  Nil <> x = x
  x <> Nil = x
  (Cons x xs) <> y = Cons x $ xs <> y

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure x = Cons x mempty
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Failure e2) = Failure $ e <> e2
  _ <*> (Failure e) = Failure e
  (Failure e) <*> _ = Failure e
  (Success fa) <*> (Success a) = Success $ fa a