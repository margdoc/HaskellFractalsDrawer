module Main where
import SFML.Graphics
    ( white,
      createRenderWindow,
      appendVA,
      createVA,
      setPrimitiveType,
      PrimitiveType(LineStrip),
      SFRenderTarget(drawVertexArray),
      RenderWindow,
      VertexArray,
      Vertex(..),
      display,
      waitEvent, renderStates, idTransform, RenderStates (transform, RenderStates), translation, scaling, rotationWithCenter, fontFromFile, Transform )
import SFML.Window
    ( Vec2f(Vec2f),
      ContextAttribute(ContextDefault),
      ContextSettings(ContextSettings),
      SFEvent(SFEvtClosed),
      VideoMode(VideoMode),
      WindowStyle(SFDefaultStyle),
      destroy )
import Geometry
    ( vertexFromPoint, Drawable (getDrawable), Lines )
import KochFlake ( kochFlake )
import qualified BasicFractal as BF
import PythagoreanTree ( generateTree, towerGenerator, splitGenerator, PythagoreanTree )
import qualified Fractal as F
import SFML.Utils (err)

width :: Int
width = 640
height :: Int
height = 640

fractal :: Lines
fractal = kochFlake 7

tree :: PythagoreanTree
tree = generateTree splitGenerator 5

fontPath = "assets/DejaVuSansMono.ttf"

main :: IO ()
main = do
    putStr . show $ ()
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode Main.width Main.height 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    font <- err $ fontFromFile fontPath
    va <- getDrawable tree
    loop wnd va
    destroy va
    destroy wnd

treeTransform :: Transform
treeTransform = 
        translation 0 height *
        rotationWithCenter 180 x_center 0 *
        translation (x_center - 50) 0 *
        scaling 100 100
    where 
        x_center = fromIntegral Main.width / 2
        height = fromIntegral Main.height

loop :: RenderWindow -> VertexArray -> IO ()
loop wnd va = do
    drawVertexArray wnd va $ Just (renderStates { 
        transform = treeTransform
    })
    display wnd
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd va
