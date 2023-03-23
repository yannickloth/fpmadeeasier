module Chapters.Ch7a
  ( Either(..)
  , Maybe(..)
  , test
  ) where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

import Prelude (Ordering, Unit, discard, show, ($), (<), (<=), (>=), (==), (>), (||))
import Prim.Ordering (GT, LT)

data Maybe a = Nothing | Just a

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just x) (Just y) = x == y
--   eq _ _ = false

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare (Just x) (Just y) = compare x y
--   compare Nothing _ = LT
--   compare _ Nothing = GT

-- greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
-- greaterThanOrEq x y = cmp == GT || cmp == EQ || cmp == LT
--   where
--   cmp = compare x y

-- infixl 4 greaterThanOrEq as >=

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just x) = "(Just " <> show x <> ")"

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _ -- this is necessary for showMaybe
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "------------------"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  log "------------------"
  log $ show (Left "left" :: Either _ Unit)
  log $ show (Right (Just 42) :: Either Unit _)

