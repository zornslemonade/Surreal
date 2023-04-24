module Game(Game) where

import Data.Set as S
import Data.List as L

data Game = Game {left :: S.Set Game, right :: S.Set Game} deriving (Eq, Ord)

game l r = Game (fromList l) (fromList r)

instance Show Game where
    show (Game {left=l, right=r}) = "{ "++ L.intercalate ", " (show <$> toList l) ++ " | " ++ L.intercalate ", " (show <$> toList r) ++ " }"

zero = game [] []
one = game [zero] []
two = game [one] []
three = game [two] []

star = game [zero] [zero]



infix 4 *<=*
(*<=*) :: Game -> Game -> Bool
g *<=* h = all (*<!* h) (left g) && all (g *<!*) (right h)

infix 4 *>=*
(*>=*) :: Game -> Game -> Bool
(*>=*) = flip (*<=*)

infix 4 *==*
(*==*) :: Game -> Game -> Bool
g *==* h = g *>=* h && g *<=* h

infix 4 *<!*
(*<!*) :: Game -> Game -> Bool
(*<!*) = (not .) . (*>=*)

infix 4 *>!*
(*>!*) :: Game -> Game -> Bool
(*>!*) = flip (*<!*)

infix 4 *!!*
(*!!*) :: Game -> Game -> Bool
g *!!* h = g *>!* h && g *<!* h

infixl 6 *+*
(*+*) :: Game -> Game -> Game
g *+* h = Game (S.map (g *+*) (left h) `S.union` S.map (*+* h) (left g)) (S.map (g *+*) (right h) `S.union` S.map (*+* h) (right g))

infixl 6 *-*
(*-*) :: Game -> Game -> Game
g *-* h = g *+* neg h

neg :: Game -> Game
neg (Game {left=l, right=r}) = Game (S.map neg r) (S.map neg l)