-- | Module for chapter 5 of Functional Programming Made Easier: A Step-by-Step Guide
module Ch5
  ( (!!)
  , (#)
  , ($)
  , apply
  , applyFlipped
  , catMaybes
  , concat
  , const
  , drop
  , dropEnd
  , dropWhile
  , filter
  , findIndex
  , findLastIndex
  , flip
  , head
  , index
  , init
  , last
  , length
  , null
  , range
  , reverse
  , singleton
  , snoc
  , tail
  , take
  , takeEnd
  , takeWhile
  , test
  , uncons
  ) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<), (>), (==), (>=), (<=), (<<<), (>>>))

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

{- | Takes a list of lists and returns a single list containing all elements 
     of all lists in the same order.
-}
concat :: ∀ a. List (List a) -> List (a)
concat Nil = Nil
concat (x : Nil) = x
{- case of an empty list in our list of lists, 
   and case of when we have exhausted the head of our list of lists: at the end of each list, there is a Nil
-}
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

{- | Filters out elements of the specified list for which the specified predicate does not hold.
-}
filter :: ∀ a. (a -> Boolean) -> List a -> List a

-- Simple implementation, time-efficient, but not space/memory-efficient
-- filter _ Nil = Nil
-- filter predicate (x : xs)
--   | predicate x = x : filter predicate xs
--   | otherwise = filter predicate xs

-- A tail-recursive implementation: space/memory-efficient, but not time-efficient because of the reversal of the resulting list
filter predicate = reverse <<< go Nil
  where
  go filtered Nil = filtered
  go filtered (x : xs)
    | predicate x = go (x : filtered) xs
    | otherwise = go filtered xs

{- | Takes a list of Maybe's, filters out the Nothings and unwraps the Just a's, returning a list of present a's.
-}
catMaybes :: ∀ a. List (Maybe a) -> List a
-- Not tail-recursive:
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

{- | Creates a list with a sequential set of numbers.
-}
range :: Int -> Int -> List Int
range from to = go Nil to from --here we go from to to from so that the cons infra will create the list in the right order
  where
  go rl start end
    | start == end = start : rl
    | otherwise = go (start : rl) (start - step) end
  step = if from < to then 1 else -1

take :: ∀ a. Int -> List a -> List a
-- Not tail recursive:
-- take _ Nil = Nil
-- take n (x : xs)
--   | (n <= 0) = Nil
--   | otherwise = x : take (n - 1) xs

-- Tail recursive:
take n = reverse <<< go Nil (max 0 n) -- max 0 n makes sure that the quantity to take is positive
  where
  go nl _ Nil = nl
  go nl 0 _ = nl
  go nl q (x : xs) = go (x : nl) (q - 1) xs

{- | Removes the specified amount of elements from the specified list.
-}
drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop n (x : xs)
  | n <= 0 = (x : xs)
  | otherwise = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile predicate = reverse <<< go Nil
  where
  go acc Nil = acc
  go acc (x : xs)
    | predicate x = go (x : acc) xs
    | otherwise = acc

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile predicate l@(x : xs)
  | predicate x = dropWhile predicate xs
  | otherwise = l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \tup@(Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else tup

-- equivalent to line above, just using $ instead of #: 
-- go (x : xs) = (\tup@(Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else tup) $ go xs

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) = unzip ts # (\(Tuple xs ys) -> Tuple (x : xs) (y : ys))

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

  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)

  log $ show $ filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil)

  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)

  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ range 3 3

  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)

  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop (-2) (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)

  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)

  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)

  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)

  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)

  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)

  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))