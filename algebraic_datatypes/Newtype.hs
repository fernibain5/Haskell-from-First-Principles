-- Newtype.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Example1 (Int, String) deriving (Eq, Show)

instance TooMany (Int, String) where
  tooMany (a , b) = 





