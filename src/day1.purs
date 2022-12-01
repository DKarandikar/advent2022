module Day1 where

import Prelude(show, ($))

import Data.String (split, Pattern(..))

import Data.Int(fromString)
import Data.Array(snoc)

import Data.Foldable

import Data.Maybe (Maybe(..))

type AState = {
    current :: Array Int,
    soFar :: Array Int
}

emptyState :: AState
emptyState = {current: [], soFar: []}

exec1a:: String -> String
exec1a s = show $ maximum $ (\x -> x.soFar) $ resolveFinalState $ foldl fold1a emptyState (split (Pattern ("\n")) s)


fold1a:: AState -> String -> AState
fold1a state s = 
    case s of
        "" -> {current: [], soFar: snoc state.soFar (sum state.current)}
        x ->  case fromString x of 
                Just y -> {current: snoc state.current y, soFar: state.soFar}
                Nothing -> state

resolveFinalState :: AState -> AState
resolveFinalState state = {current: [], soFar: snoc state.soFar (sum state.current)}