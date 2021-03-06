{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Geometry ( Point, Line, Path, Lines, polygonFromLines, vertexFromPoint, transformLine, scaleLine, rotateLine, revertLine, Drawable(getDrawable), addLineToVA, addColorLineToVA ) where
import SFML.Graphics ( RenderWindow, SFDrawable, Vertex (..), white, SFDrawable(draw), SFRenderTarget (drawVertexArray), createVA, setPrimitiveType, PrimitiveType (LineStrip, Lines), appendVA, VertexArray, RenderStates (transform), Transform, transformPoint, scalingWithCenter, rotationWithCenter, Color )
import SFML.Window ( destroy, Vec2f(Vec2f) )

type Point = Vec2f
type Line = (Vec2f, Vec2f)
type Path = [Vec2f]

type Lines = [Line]

polygonFromLines :: Lines -> Path
polygonFromLines = map fst

colorVertexFromPoint :: Color -> Point -> Vertex
colorVertexFromPoint color p = Vertex {
        SFML.Graphics.position = p,
        color = color,
        texCoords = p
    }

vertexFromPoint :: Point -> Vertex
vertexFromPoint = colorVertexFromPoint white

transformLine :: Transform -> Line -> Line
transformLine t (p1, p2) = (transformPoint t p1, transformPoint t p2)

scaleLine :: Point -> Float -> Line -> Line
scaleLine (Vec2f x y) scale =
    transformLine transform
    where transform = scalingWithCenter scale scale x y

rotateLine :: Point -> Float -> Line -> Line
rotateLine (Vec2f x y) angle =
    transformLine transform
    where transform = rotationWithCenter angle x y

revertLine :: Line -> Line
revertLine (p1, p2) = (p2, p1)

class (SFDrawable b) => Drawable a b where
  getDrawable :: a -> IO b

addLineToVA :: VertexArray -> Line -> IO ()
addLineToVA va (p1, p2) = do 
  append p1
  append p2
  where append = appendVA va . vertexFromPoint

addColorLineToVA :: Color -> VertexArray -> Line -> IO ()
addColorLineToVA color va (p1, p2) = do 
  append p1
  append p2
  where append = appendVA va . colorVertexFromPoint color

instance Drawable Lines VertexArray where
  getDrawable lines = do
        va <- createVA
        setPrimitiveType va Lines
        mapM_ (addLineToVA va) lines
        return va
