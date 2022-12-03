module Day2 where

import Data.String

import Data.Foldable

import Prelude

exec2a:: String -> String
exec2a s = show $ foldl fold2a 0 (split (Pattern ("\n")) s)

exec2b:: String -> String
exec2b s =  show $ foldl fold2b 0 (split (Pattern ("\n")) s)

fold2a :: Int -> String -> Int
fold2a x row = x + case row of 
    "A X" -> 1 + 3
    "A Y" -> 2 + 6
    "A Z" -> 3 + 0
    "B X" -> 1 + 0
    "B Y" -> 2 + 3
    "B Z" -> 3 + 6
    "C X" -> 1 + 6
    "C Y" -> 2 + 0
    "C Z" -> 3 + 3
    _ -> 0 


fold2b :: Int -> String -> Int
fold2b x row = x + case row of 
    "A X" -> 3 + 0
    "A Y" -> 1 + 3
    "A Z" -> 2 + 6
    "B X" -> 1 + 0
    "B Y" -> 2 + 3
    "B Z" -> 3 + 6
    "C X" -> 2 + 0
    "C Y" -> 3 + 3
    "C Z" -> 1 + 6
    _ -> 0 