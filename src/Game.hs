module Game(Game) where

import Data.Set as S
import Data.List as L

-- | A game is an ordered pair of sets of games: a left set and a right set.
data Game = Game {left :: S.Set Game, right :: S.Set Game} deriving (Eq, Ord)

-- | Alternate constructor for games
game :: [Game] -> [Game] -> Game
game l r = Game (fromList l) (fromList r)

-- | Games are displayed in Conway notation
instance Show Game where
    show (Game {left=l, right=r}) = "{ "++ L.intercalate ", " (show <$> toList l) ++ " | " ++ L.intercalate ", " (show <$> toList r) ++ " }"

-- | Commonly used games
zero = game [] []
one = game [zero] []
two = game [one] []
three = game [two] []
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

-- | Addition of games
infixl 6 *+*
(*+*) :: Game -> Game -> Game
g *+* h =
    let gl = toList $ left g
        gr = toList $ right g
        hl = toList $ left h
        hr = toList $ right h
        zl = [a *+* h | a <- gl] ++ [g *+* b | b <- hl]
        zr = [a *+* h | a <- gr] ++ [g *+* b | b <- hr]
    in game zl zr

-- | Subtraction of games
infixl 6 *-*
(*-*) :: Game -> Game -> Game
g *-* h = g *+* neg h

-- | Additive inverse of a game
neg :: Game -> Game
neg g = Game (S.map neg $ right g) (S.map neg $ left g)

-- | Multiplication of games
infixl 7 ***
(***) :: Game -> Game -> Game
g *** h =
    let gl = toList $ left g
        gr = toList $ right g
        hl = toList $ left h
        hr = toList $ right h
        zl = [a *** h *+* g *** b *-* a *** b | a <- gl, b <- hl] ++ [a *** h *+* g *** b *-* a *** b | a <- gr, b <- hr]
        zr = [a *** h *+* g *** b *-* a *** b | a <- gl, b <- hr] ++ [a *** h *+* g *** b *-* a *** b | a <- gr, b <- hl]
    in game zl zr

-- | Checks if a game is numeric
isSurreal :: Game -> Bool
isSurreal g = and [isSurreal x && isSurreal y && x *<!* y | x <- toList $ left g, y <- toList $ right g]