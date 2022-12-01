module Day1 where

import Prelude(show, ($), (<), (/=))

import Data.String (split, Pattern(..))

import Data.Int(fromString)
import Data.Array(snoc, filter)

import Data.Foldable

import Data.Maybe (Maybe(..))

type AState = {
    current :: Array Int,
    soFar :: Array Int
}

emptyState :: AState
emptyState = {current: [], soFar: []}

exec1a:: String -> String
exec1a s = show $ maximum $ getAllTotals s

exec1b:: String -> String
exec1b s = show $ sum $ foldl fold1b [] (getAllTotals s)

getAllTotals:: String -> Array Int
getAllTotals s = (\x -> x.soFar) $ resolveFinalState $ foldl fold1a emptyState (split (Pattern ("\n")) s)


fold1a:: AState -> String -> AState
fold1a state s = 
    case s of
        "" -> {current: [], soFar: snoc state.soFar (sum state.current)}
        x ->  case fromString x of 
                Just y -> {current: snoc state.current y, soFar: state.soFar}
                Nothing -> state

resolveFinalState :: AState -> AState
resolveFinalState state = {current: [], soFar: snoc state.soFar (sum state.current)}

fold1b:: Array Int -> Int -> Array Int
fold1b maxSoFar n = if (length maxSoFar) < 3 then snoc maxSoFar n else filter (\x -> cheakyMin x (snoc maxSoFar n )) (snoc maxSoFar n )

cheakyMin :: Int -> Array Int -> Boolean
cheakyMin x array = case minimum array of 
    Just y -> x /= y
    Nothing -> false
