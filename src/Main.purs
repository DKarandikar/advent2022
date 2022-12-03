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
import Day2 (exec2a, exec2b)
import Day3 (exec3a, exec3b)
import Day4 (exec4a, exec4b)
import Day5 (exec5a, exec5b)
import Day6 (exec6a, exec6b)
import Day7 (exec7a, exec7b)
import Day8 (exec8a, exec8b)
import Day9 (exec9a, exec9b)
import Day10 (exec10a, exec10b)
import Day11 (exec11a, exec11b)
import Day12 (exec12a, exec12b)
import Day13 (exec13a, exec13b)
import Day14 (exec14a, exec14b)
import Day15 (exec15a, exec15b)
import Day16 (exec16a, exec16b)
import Day17 (exec17a, exec17b)
import Day18 (exec18a, exec18b)
import Day19 (exec19a, exec19b)
import Day20 (exec20a, exec20b)
import Day21 (exec21a, exec21b)
import Day22 (exec22a, exec22b)
import Day23 (exec23a, exec23b)
import Day24 (exec24a, exec24b)
import Day25 (exec25a, exec25b)


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
    "2a" -> exec2a file
    "2b" -> exec2b file
    "3a" -> exec3a file
    "3b" -> exec3b file
    "4a" -> exec4a file
    "4b" -> exec4b file
    "5a" -> exec5a file
    "5b" -> exec5b file
    "6a" -> exec6a file
    "6b" -> exec6b file
    "7a" -> exec7a file
    "7b" -> exec7b file
    "8a" -> exec8a file
    "8b" -> exec8b file
    "9a" -> exec9a file
    "9b" -> exec9b file
    "10a" -> exec10a file
    "10b" -> exec10b file
    "11a" -> exec11a file
    "11b" -> exec11b file
    "12a" -> exec12a file
    "12b" -> exec12b file
    "13a" -> exec13a file
    "13b" -> exec13b file
    "14a" -> exec14a file
    "14b" -> exec14b file
    "15a" -> exec15a file
    "15b" -> exec15b file
    "16a" -> exec16a file
    "16b" -> exec16b file
    "17a" -> exec17a file
    "17b" -> exec17b file
    "18a" -> exec18a file
    "18b" -> exec18b file
    "19a" -> exec19a file
    "19b" -> exec19b file
    "20a" -> exec20a file
    "20b" -> exec20b file
    "21a" -> exec21a file
    "21b" -> exec21b file
    "22a" -> exec22a file
    "22b" -> exec22b file
    "22a" -> exec22a file
    "22b" -> exec22b file
    "23a" -> exec23a file
    "23b" -> exec23b file
    "24a" -> exec24a file
    "24b" -> exec24b file
    "25a" -> exec25a file
    "25b" -> exec25b file
    _ -> "Invalid"
