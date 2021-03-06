{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PythagoreanTree ( PythagoreanTree, generateTree, towerGenerator, splitGenerator ) where
import Geometry ( Point, Line, Lines, rotateLine, scaleLine, revertLine, Drawable(getDrawable), addLineToVA, addColorLineToVA )
import Fractal ( DoubleMonad(..), Fractal(..) )
import Data.List ( concat )
import SFML.Window (Vec2f(Vec2f))
import SFML.Graphics (VertexArray(VertexArray), createVA, setPrimitiveType, PrimitiveType (Lines), appendVA, red, blue)

data Direction = Up | Down
data Vector = Vector !Line !Direction

type Square = Line

type Generator = Line -> [Vector]

type PythagoreanTree = ([Square], [Line])

instance Fractal [] Square Square where

squareSides :: Square -> [Line]
squareSides line@(p1, p2) = [
        scaleLine p1 scale . rotateLine p1 45 $ line,
        scaleLine p2 scale . rotateLine p2 45 $ line
    ]
    where
        scale = 1 / sqrt 2

instance Drawable PythagoreanTree VertexArray where
    getDrawable (squares, lines) = do
        va <- createVA
        setPrimitiveType va Lines
        mapM_ (addLineToVA va) . concatMap squareSides $ squares
        mapM_ (addColorLineToVA blue va) squares
        mapM_ (addColorLineToVA red va)  lines
        return va

    
topSquare :: Square -> Line
topSquare line@(_, p2) =
    scaleLine p2 (1 / sqrt 2) . rotateLine p2 (-45) $ line


newSquare :: Vector -> Square
newSquare (Vector line@(p1, _) direction) =
    scaleLine p1 (sqrt 2) . rotateLine p1 angle $ line
    where
        angle = case direction of
            Up -> 45
            Down -> -45


generateTree :: Generator -> Int -> PythagoreanTree
generateTree generator =
    gen initial (mapperOld, mapperNew)
    where
        initial = ([], [(Vec2f 0 0, Vec2f 1 0)])
        mapperOld square = ([square], [])
        mapperNew line = (newSquares, map topSquare newSquares)
            where
                newSquares = map newSquare $ generator line


-- Examples

towerGenerator :: Generator
towerGenerator line = [Vector line Up]

splitGenerator :: Generator
splitGenerator line@(p1, p2) = [
        newLine . scaleLine p1 scale . rotateLine p1 45 $ line,
        newLine . scaleLine p2 scale . rotateLine p2 (-45) $ line
    ]
    where
        scale = 1 / sqrt 2
        newLine line = Vector line Up
