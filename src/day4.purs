module Day4 where

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
exec4a s = show $ foldl (\x y -> x + y) 0 (map (scoreLine completeOverlap) (split (Pattern ("\n")) s))

exec4b:: String -> String
exec4b s = show $ foldl (\x y -> x + y) 0 (map (scoreLine partialOverlap) (split (Pattern ("\n")) s))


scoreLine :: (Tuple (Array Int) (Array Int) -> Int) -> String -> Int
scoreLine score s = score $ Tuple 
    (map unsafeFromString (split (Pattern "-") (unsafeI (split (Pattern (",")) s) 0 ))) 
    (map unsafeFromString (split (Pattern "-") (unsafeI (split (Pattern (",")) s) 1 )))


completeOverlap :: Tuple (Array Int) (Array Int) -> Int
completeOverlap t 
    | (unsafeI (fst t) 0) <= (unsafeI (snd t) 0) && (unsafeI (fst t) 1) >= (unsafeI (snd t) 1) = 1
    | (unsafeI (fst t) 0) >= (unsafeI (snd t) 0) && (unsafeI (fst t) 1) <= (unsafeI (snd t) 1) = 1
    | otherwise = 0


partialOverlap :: Tuple (Array Int) (Array Int) -> Int
partialOverlap t 
    | (unsafeI (fst t) 0) >= (unsafeI (snd t) 0) && (unsafeI (fst t) 0) <= (unsafeI (snd t) 1) = 1
    | (unsafeI (fst t) 1) >= (unsafeI (snd t) 0) && (unsafeI (fst t) 1) <= (unsafeI (snd t) 1) = 1
    | otherwise = completeOverlap t

unsafeFromString :: String -> Int
unsafeFromString x = case fromString x of 
    Just y -> y 
    _ -> error "Invalid string"

unsafeI = unsafePartial $ unsafeIndex 