-- OptionalMonoid.hs
module OptionalMonoid where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) (Only a) Nada = (Only a) 
  (<>) Nada (Only a) = (Only a)
  (<>) (Only a) (Only b) = (Only (a `mappend` b))


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                  -> Adverb
                  -> Noun
                  -> Adjective
                  -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he said ",
          adv, " as he jumped into his car ",
          noun, " and drove off with his ",
          adj, " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a