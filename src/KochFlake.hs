{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module KochFlake ( kochFlake ) where
import BasicFractal
    ( BasicFractal(..) )
import Geometry ( Line, Lines, transformLine, Point, scaleLine, revertLine, rotateLine )
import SFML.System (Vec2f(Vec2f))

type KochFlake = Lines

firstIteration :: Lines
firstIteration = [
        (p1, p2),
        (p2, p3),
        (p3, p1)
    ]
    where
        h1 = sqrt 3 / 6
        h2 = h1 * 2
        p1 = Vec2f (-0.5) (-h1)
        p2 = Vec2f 0.5 (-h1)
        p3 = Vec2f 0 h2

newIteration :: (Point, Point) -> Lines
newIteration line@(p1, p2) = [l1, l2, l3, l4]
    where 
        l1 = scaleLine p1 (1 / 3) line
        l2 = revertLine $ rotateLine (snd l1) 120 l1
        l3 = revertLine $ rotateLine (fst l4) (-120) l4
        l4 = scaleLine p2 (1 / 3) line

instance BasicFractal [] Line where

kochFlake :: Int -> [Line]
kochFlake = gen firstIteration newIteration