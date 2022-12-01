module Main where

import Prelude

import Data.Array (head, drop)
import Data.Maybe (Maybe(..))
import Day1 (exec1a)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)


main :: Effect Unit
main = do
  args <- argv
  log $ execute $ head $ drop 2 args


execute :: (Maybe String) -> String
execute (Just "1a") = exec1a
execute _ = "Invalid"