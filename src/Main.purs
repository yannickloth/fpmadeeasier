module Main
  ( main
  ) where

import Chapters.Ch5 (test) as Ch5
import Chapters.Ch5 (($))
import Chapters.Ch7a (test) as Ch7a
import Chapters.Ch7b (test) as Ch7b
import Effect (Effect)

import Effect.Console (log)
import Prelude (Unit, show)

main :: Effect Unit
-- main = Ch5.test
-- main = Ch7a.test
main = Ch7b.test
-- main = do
--   log $ show $ "hello"

