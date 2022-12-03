module Day3 where

import Data.Char
import Data.String
import Prelude

import Data.Array (intersect, head)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toChar, toCharArray)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

exec3a:: String -> String
exec3a s = show $ foldl (\x y -> x + y) 0 (map scoreLine (split (Pattern ("\n")) s))

exec3b:: String -> String
exec3b s = s 

scoreLine :: String -> Int
scoreLine s = case head $ intersect 
        (toCharArray (take (length s / 2) s)) 
        (toCharArray (drop (length s / 2) s))
    of
    Just x -> scoreChar x
    _ -> error "Sad"

scoreChar :: Char -> Int
scoreChar c 
    | toCharCode c < 91 =  ((toCharCode c) - 64) + 26
    | otherwise  = (toCharCode c) - 96