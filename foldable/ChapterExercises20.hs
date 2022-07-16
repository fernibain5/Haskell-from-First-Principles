module ChapterExercises20 where

import Data.Monoid

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
{- minimumFoldMap :: (Foldable t, Ord a) => t a -> Maybe a
minimumFoldMap = foldMap (minimum (t a)) (t a) -}

