module ChapterExercises20 where

import Data.Monoid
import Data.Foldable (toList)

-- 1
sumFoldMap :: (Foldable t, Num a) => t a -> a
sumFoldMap = getSum . foldMap Sum

sumFoldr :: (Foldable t, Num a) => t a -> a
sumFoldr = foldr (+) 0

--2
productFoldMap :: (Foldable t, Num a) => t a -> a
productFoldMap = getProduct . foldMap Product

productFoldr :: (Foldable t, Num a) => t a -> a
productFoldr = foldr (*) 1

-- 3
elemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap c = getAny . foldMap (\x -> if x == c
                                        then Any True
                                        else Any False)

elemFoldr :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr c = foldr (\x g -> (x == c) || g) False

-- 4
-- minimumFoldMap :: (Foldable t, Ord a) => t a -> Maybe a
-- minimumFoldMap =  (foldMap Just)  

minimumFoldr :: (Foldable t, Ord a) => t a -> Maybe a
minimumFoldr = foldr (\x g-> case g of
                                   Nothing -> Just x
                                   Just x' -> Just $ min x x')
                     Nothing

-- 5                     
maximumFoldr :: (Foldable t, Ord a) => t a -> Maybe a
maximumFoldr = foldr (\x g -> case g of
                                   Nothing -> Just x
                                   Just x' -> Just $ max x x')
                     Nothing

-- 6
nullFoldMap :: (Foldable t) => t a -> Bool
nullFoldMap = getAll . foldMap (const (All False)) 

nullFoldr :: (Foldable t) => t a -> Bool
nullFoldr = foldr (\_ _ -> False) True 

-- 7
lengthFoldMap :: (Foldable t) => t a -> Int
lengthFoldMap = getSum  . foldMap (const (Sum 1))

lengthFoldr :: (Foldable t) => t a -> Int
lengthFoldr = foldr (\_ g -> 1 + g) 0

-- 8
toListFoldMap :: (Foldable t) => t a -> [a]
toListFoldMap = foldMap (: [])

toListFoldr :: (Foldable t) => t a -> [a]
toListFoldr = foldr (:) [] 

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' =  foldr (<>) mempty

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x g -> (<>) (f x) g) mempty



-- Write Foldable instances for the following datatypes.

-- 1
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant b) where
  foldMap f (Constant x) = f x


-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two b) where
  foldMap f (Two _ b) =  f b

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

{- Thinking cap time. Write a filter function for Foldable types
using foldMap. -}

filterF :: ( Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)