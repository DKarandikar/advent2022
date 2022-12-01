module Main where

import Prelude

import Data.Array (head, drop)
import Data.Maybe (Maybe(..))
import Data.String.Utils (stripChars)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)

import Day1 (exec1a, exec1b)


main :: Effect Unit
main = do
  args <- argv

  result <- readTextFile UTF8 ("days/" <> (stripChars "ab" $ extract args) <> ".txt")
  log $ (execute (extract args) result)

extract :: Array String -> String
extract args = 
  case (head $ drop 2 args) of 
    Just x -> x
    _ -> "invalid"

execute :: String -> String -> String
execute x file = 
  case x of 
    "1a" -> exec1a file
    "1b" -> exec1b file
    _ -> "Invalid"
