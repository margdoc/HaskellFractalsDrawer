{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PythagoreanTree ( PythagoreanTree, generateTree, towerGenerator, pointGenerator, symetricPointGenerator ) where
import Geometry ( Point, Line, Lines, rotateLine, scaleLine, revertLine, Drawable(getDrawable), addLineToVA, addColorLineToVA )
import Fractal ( DoubleMonad(..), Fractal(..) )
import Data.List ( concat )
import SFML.Window (Vec2f(Vec2f))
import SFML.Graphics (VertexArray(VertexArray), createVA, setPrimitiveType, PrimitiveType (Lines), appendVA, red, blue, white)

data Direction = Up | Down


oppositeDirection :: Direction -> Direction
oppositeDirection direction = case direction of
    Up -> Down
    Down -> Up

data Vector = Vector !Line !Direction

data Square = Square !Line !Direction

type Generator = Vector -> [Vector]

type PythagoreanTree = ([Square], [Vector])

instance Fractal [] Square Vector where

squareSides :: Square -> [Line]
squareSides (Square line@(p1, p2) direction) = [
        scaleLine p1 scale . rotateLine p1 angle $ line,
        scaleLine p2 scale . rotateLine p2 angle $ line
    ]
    where
        scale = 1 / sqrt 2
        angle = case direction of
            Up -> 45
            Down -> -45

instance Drawable PythagoreanTree VertexArray where
    getDrawable (squares, lines) = do
        va <- createVA
        setPrimitiveType va Lines
        mapM_ (addLineToVA va) . concatMap squareSides $ squares
        --mapM_ (addColorLineToVA blue va . (\(Square line _) -> line)) squares
        mapM_ (addColorLineToVA white va . (\(Vector line _) -> line)) lines
        return va

    
topSquare :: Square -> Vector
topSquare (Square line@(p1, p2) direction) =
    Vector (scaleLine p2 (1 / sqrt 2) . rotateLine p2 angle $ line) direction
    where
        angle = case direction of
            Up -> -45
            Down -> 45


newSquare :: Vector -> Square
newSquare (Vector line@(p1, _) direction) =
    Square (scaleLine p1 (sqrt 2) . rotateLine p1 angle $ line) direction
    where
        angle = case direction of
            Up -> 45
            Down -> -45

generateTree :: Generator -> Int -> PythagoreanTree
generateTree generator =
    gen initial (mapperOld, mapperNew)
    where
        initial = ([], [Vector (Vec2f 0 0, Vec2f 1 0) Up])
        mapperOld square = ([square], [])
        mapperNew vector = (newSquares, map topSquare newSquares)
            where
                newSquares = map newSquare $ generator vector


-- Examples

towerGenerator :: Generator
towerGenerator vector = [vector]

pointGenerator :: Float -> Generator
pointGenerator 0 vector = towerGenerator vector
pointGenerator 180 vector = towerGenerator vector

pointGenerator angle (Vector line@(p1, p2) _) = [
        newLine (p1, p),
        newLine (p, p2)
    ]
    where
        scaled@(_, middle) = scaleLine p1 0.5 line 
        (p, _) = rotateLine middle (-angle) scaled
        newLine line = Vector line Up


symetricPointGenerator :: Float -> Generator
symetricPointGenerator 0 vector = towerGenerator vector
symetricPointGenerator 180 vector = towerGenerator vector

symetricPointGenerator angle (Vector line@(p1, p2) direction) = [
        Vector (p1, p) direction,
        Vector (p2, p) (oppositeDirection direction)
    ]
    where
        scaled@(_, middle) = scaleLine p1 0.5 line
        rotation_angle = case direction of
            Up -> -angle
            Down -> angle
        (p, _) = rotateLine middle rotation_angle scaled