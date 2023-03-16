module Main where

import Prelude (Unit)
import Ch5 (test) as Ch5
import Effect (Effect)

main :: Effect Unit
main = Ch5.test
