-- | Module for chapter 5 of Functional Programming Made Easier: A Step-by-Step Guide
module Ch5
  ( (!!)
  , (#)
  , ($)
  , apply
  , applyFlipped
  , const
  , findIndex
  , findLastIndex
  , flip
  , head
  , index
  , init
  , last
  , length
  , null
  , reverse
  , singleton
  , snoc
  , tail
  , test
  , uncons
  ) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, negate, otherwise, show, (+), (-), (<), (>=), (/=), (==), type (~>))

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as # -- left means that the function on the right is applied to the parameters on the left

const :: ∀ a b. a -> b -> a
const x _ = x

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
-- "flip const 1 2" is the same as "const 2 1": it flips the parameters of the specified function.
-- In "flip const 1 2", "const", "1" and "2" are parameters of "flip", which will call "const" applied to "2" and "1".
flip f = \x y -> f y x

singleton :: ∀ a. a -> List a
singleton a = a : Nil

{- | Checks if the specified list is empty.
-}
null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

{- | Inverse of Cons: instead of appending at the head of the list, this appends at the end/tail of the list.
-}
snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x -- the Cons operator ':' adds one single element to the head of the list, thus 'x' is this single element and 'xs' is a list.

{- | Computes the length of the specified list
-}
length :: ∀ a. List a -> Int
length l = go 0 l --the accumulator starts at 0, then each single element of the list increases the value of the accumulator
  -- the name go of the helper function is idiomatic in FP - this tradition started in Haskell, no idea why.
  where
  go :: ∀ a. Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

{- | Returns the head of the list
-}
head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

{- | Returns the tail of the list
-}
tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

{- | Returns the last element of the list
-}
last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

{- | Returns the init of the list: the list before the last element.
-}
init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go Nil = Nil --is never used, but necessary to satisfy the compiler, as the list type accepts empty lists
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

{- | Destructures a list into a Record with its head and its tail
-}
uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just ({ tail: xs, head: x })

{- | Returns the value at the specified index. Returns nothing if the index is negative or the list is empty.
     The parameters seem backwards so that the infix operator '!!' has the better syntax with everything in the right order.
-}
index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing --using a guard, handling the case where the specified index is negative
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixr 8 index as !!

{- | Finds the index of the element of the specified list for which the specified predicate holds.
-}
findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred list = go 0 list
  where
  go _ Nil = Nothing
  go i (x : xs) = if pred x then Just i else go (i + 1) xs

{- | Finds the index of the last element of the specified list for which the specified predicate holds.
-}
findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred list = go 0 Nothing list
  where
  {- | go(currentIndex,lastIndex,List(a)) -> Maybe Int
  -}
  go :: Int -> Maybe Int -> List a -> Maybe Int
  go _ lastIndex Nil = lastIndex
  go currentIndex lastIndex (x : xs) =
    go (currentIndex + 1) (if pred x then Just (currentIndex) else lastIndex) xs

{- | Reverses the specified list
-}
reverse :: List ~> List -- ~> is a binary operator on types, not on values.
reverse Nil = Nil
reverse (x : Nil) = (x : Nil)
reverse (x : xs) = snoc (reverse xs) x

test :: Effect Unit
test = do
  log (show (flip const 1 2))
  log $ show $ flip const 1 2
  flip const 1 2 # show # log

  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)

  log $ show $ snoc (1 : 2 : Nil) 3

  log $ show $ length Nil
  log $ show $ length (Nil : Nil)
  log $ show $ length (1 : 2 : 3 : Nil)

  log $ show $ head (Nil :: List Unit) --here we specify that the type of Nil is List
  log $ show (head Nil :: Maybe Unit) --here we specify the return type of head, which is a Maybe, and more concretely a Nothing in the case of Nil
  log $ show $ head ("abc" : "123" : Nil)

  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)

  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)

  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)

  log $ show $ uncons (1 : 2 : 3 : Nil)

  log $ show $ index (1 : 2 : 3 : Nil) (0)
  log $ show $ index (1 : 2 : 3 : Nil) (1)
  log $ show $ index (1 : 2 : 3 : Nil) (2)
  log $ show $ index (1 : 2 : 3 : Nil) (3)
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1

  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)

  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)

  log $ show $ reverse (10 : 20 : 30 : Nil)
  