{-# LANGUAGE InstanceSigs #-}

module Game (Game) where

import Data.List as L
import Data.Set as S

-- | A game is an ordered pair of sets of games: a left set and a right set.
data Game = Game {left :: S.Set Game, right :: S.Set Game} deriving (Eq, Ord)

-- | Alternate constructor for games
game :: [Game] -> [Game] -> Game
game l r = Game (fromList l) (fromList r)

-- | Games are displayed in Conway notation
instance Show Game where
  show :: Game -> String
  show (Game {left = l, right = r}) = "{ " ++ L.intercalate ", " (show <$> toList l) ++ " | " ++ L.intercalate ", " (show <$> toList r) ++ " }"

-- | Commonly used games
zero :: Game
zero = game [] []

one :: Game
one = game [zero] []

two :: Game
two = game [one] []

three :: Game
three = game [two] []

star :: Game
star = game [zero] [zero]

-- | Preordering on games
infix 4 *<=*

(*<=*) :: Game -> Game -> Bool
g *<=* h = all (*<!* h) (left g) && all (g *<!*) (right h)

-- | Preordering on games
infix 4 *>=*

(*>=*) :: Game -> Game -> Bool
(*>=*) = flip (*<=*)

-- | Equivalence of games up to ordering
infix 4 *==*

(*==*) :: Game -> Game -> Bool
g *==* h = g *>=* h && g *<=* h

-- | Negation of =>
infix 4 *<!*

(*<!*) :: Game -> Game -> Bool
g *<!* h = not $ g *>=* h

-- | Negation of <=
infix 4 *>!*

(*>!*) :: Game -> Game -> Bool
(*>!*) = flip (*<!*)

-- | Incomparibility of games
infix 4 *!!*

(*!!*) :: Game -> Game -> Bool
g *!!* h = g *>!* h && g *<!* h

-- | Num instance for games
instance Num Game where
  (+) :: Game -> Game -> Game
  g + h =
    let gl = toList $ left g
        gr = toList $ right g
        hl = toList $ left h
        hr = toList $ right h
        zl = [a + h | a <- gl] ++ [g + b | b <- hl]
        zr = [a + h | a <- gr] ++ [g + b | b <- hr]
     in game zl zr

  (*) :: Game -> Game -> Game
  g * h =
    let gl = toList $ left g
        gr = toList $ right g
        hl = toList $ left h
        hr = toList $ right h
        zl = [a * h + g * b - a * b | a <- gl, b <- hl] ++ [a * h + g * b - a * b | a <- gr, b <- hr]
        zr = [a * h + g * b - a * b | a <- gl, b <- hr] ++ [a * h + g * b - a * b | a <- gr, b <- hl]
     in game zl zr

  negate :: Game -> Game
  negate g = game (negate <$> toList (right g)) (negate <$> toList (left g))

  -- | Abs leaves games which are incomparible with 0 alone
  abs :: Game -> Game
  abs g = if g *<=* 0 then negate g else g

  -- | Signum gives 0 for games which are incomparible with 0
  signum :: Game -> Game
  signum g
    | g *<=* 0 = -1
    | g *>=* 0 = 1
    | otherwise = 0

  fromInteger :: Integer -> Game
  fromInteger 0 = zero
  fromInteger n = game [fromInteger $ n - 1] []

-- | Checks if a game is numeric (i.e. a Surreal number)
isSurreal :: Game -> Bool
isSurreal g = and [isSurreal x && isSurreal y && x *<!* y | x <- toList $ left g, y <- toList $ right g]