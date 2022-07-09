-- MaybeAnotherMonoid.hs

module MaybeAnotherMonoid where

import OptionalMonoid

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = (First' (Only x))
  (<>) _ (First' (Only x)) = (First' (Only x))
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency  [ (3, return . First' $ Only a)
               , (1, return $ First' Nada) ]

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity) :: FstId)