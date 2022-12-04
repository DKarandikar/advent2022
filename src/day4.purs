module Day4
  ( error
  , exec4a
  , exec4b
  , scoreTuple
  , unsafeFromString
  , unsafeI
  )
  where

import Prelude

import Data.Foldable(foldl)
import Data.Array (unsafeIndex)
import Partial.Unsafe (unsafePartial)
import Data.Int (fromString)
import Data.String (split, Pattern(..))
import Data.Tuple (fst, snd, Tuple(..))
import Data.Maybe(Maybe(..))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

exec4a:: String -> String
exec4a s = show $ foldl (\x y -> x + y) 0 (map scoreLine (split (Pattern ("\n")) s))

exec4b:: String -> String
exec4b s = s 


scoreLine :: String -> Int
scoreLine s = scoreTuple $ Tuple 
    (map unsafeFromString (split (Pattern "-") (unsafeI (split (Pattern (",")) s) 0 ))) 
    (map unsafeFromString (split (Pattern "-") (unsafeI (split (Pattern (",")) s) 1 )))


scoreTuple :: Tuple (Array Int) (Array Int) -> Int
scoreTuple t 
    | (unsafeI (fst t) 0) <= (unsafeI (snd t) 0) && (unsafeI (fst t) 1) >= (unsafeI (snd t) 1) = 1
    | (unsafeI (fst t) 0) >= (unsafeI (snd t) 0) && (unsafeI (fst t) 1) <= (unsafeI (snd t) 1) = 1
    | otherwise = 0


unsafeFromString :: String -> Int
unsafeFromString x = case fromString x of 
    Just y -> y 
    _ -> error "Invalid string"

unsafeI = unsafePartial $ unsafeIndex 