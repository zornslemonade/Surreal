module Game(*) where

import Data.Set

newtype Game = (Set Game, Set Game) deriving (Eq, Ord)
