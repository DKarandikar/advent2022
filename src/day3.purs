module Day3 where

import Data.Char
import Data.String
import Prelude

import Data.Array (intersect, head, cons, take, drop) as Arrays
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
exec3b s = show $ foldl (\x y -> x + y) 0 (map scoreGroup (group 3 (split (Pattern ("\n")) s)))

scoreLine :: String -> Int
scoreLine s = case Arrays.head $ Arrays.intersect 
        (toCharArray (take (length s / 2) s)) 
        (toCharArray (drop (length s / 2) s))
    of
    Just x -> scoreChar x
    _ -> error "Sad"

scoreChar :: Char -> Int
scoreChar c 
    | toCharCode c < 91 =  ((toCharCode c) - 64) + 26
    | otherwise  = (toCharCode c) - 96


group :: forall a. Int -> Array a -> Array (Array a)
group _ [] = []
group n l
  | n > 0 = Arrays.cons (Arrays.take n l) (group n (Arrays.drop n l))
  | otherwise = error "Negative or zero n"


scoreGroup :: Array String -> Int
scoreGroup strings = case Arrays.head (bigIntersect (map toCharArray strings)) of 
    Just x -> scoreChar x
    _ -> error "Sad"


bigIntersect :: forall a. Eq a => Array (Array a) -> Array a
bigIntersect stuff = foldl Arrays.intersect (unsafeHead stuff) (Arrays.drop 1 stuff)

unsafeHead x = case Arrays.head x of
    Just y -> y
    _ -> error "Sad"