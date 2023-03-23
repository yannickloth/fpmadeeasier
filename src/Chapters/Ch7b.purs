module Chapters.Ch7b
  ( Age(..)
  , CSV(..)
  , FullName
  , Occupation(..)
  , Person(..)
  , test
  )
  where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String

instance showFullName :: Show FullName where
  show (FullName name) = name

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemploye

derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) =
    CSV $ show name <> "," <> show age <> "," <> show occupation

test :: Effect Unit
test = do
  log $ show $
    ( toCSV
        ( Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
        )
    ) == CSV "Sue Smith,23,Doctor"

