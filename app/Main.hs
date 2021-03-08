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
      waitEvent, renderStates, idTransform, RenderStates (transform, RenderStates), translation, scaling, rotationWithCenter, fontFromFile, Transform, getVABounds, FloatRect (..), scalingWithCenter, SFDrawable (draw), clearRenderWindow, black )
import SFML.Window
    ( Vec2f(Vec2f),
      ContextAttribute(ContextDefault),
      ContextSettings(ContextSettings),
      SFEvent(SFEvtClosed, SFEvtMouseButtonPressed),
      VideoMode(VideoMode),
      WindowStyle(SFDefaultStyle),
      destroy )
import Geometry
    ( vertexFromPoint, Drawable (getDrawable), Lines )
import KochFlake ( kochFlake )
import qualified BasicFractal as BF
import PythagoreanTree ( generateTree, towerGenerator, pointGenerator, PythagoreanTree, symetricPointGenerator )
import qualified Fractal as F
import qualified UI
import SFML.Utils ( err )

drawerWidth :: Int
drawerWidth = 640

drawerHeight :: Int
drawerHeight = 640

menuWidth :: Int
menuWidth = 150

buttonHeight :: Int
buttonHeight = 45

buttonsSpacing :: Int
buttonsSpacing = 10


fontPath :: [Char]
fontPath = "assets/DejaVuSansMono.ttf"


data FractalType = KochFlakeType | PythagoreanTreeType !Float !Bool

data State = State {
    fractalType :: FractalType,
    iteration :: Int
}


drawFractal :: State -> IO VertexArray
drawFractal state = case fractalType state of
    KochFlakeType -> getDrawable . kochFlake $ it
    PythagoreanTreeType angle False -> getDrawable . generateTree (pointGenerator angle) $ it
    PythagoreanTreeType angle True -> getDrawable . generateTree (symetricPointGenerator angle) $ it
    where
        it = iteration state


data Action = 
    NextIteration | 
    PreviousIteration | 
    Iteration Int | 
    NewType FractalType |
    IncreaseAngle |
    DecreaseAngle |
    ChangeSymetry


initialState :: State
initialState = State {
    fractalType = KochFlakeType,
    iteration = 1
}


reducer :: State -> Action -> State
reducer state NextIteration = State {
    fractalType = fractalType state,
    iteration = iteration state + 1
}

reducer state PreviousIteration = State {
    fractalType = fractalType state,
    iteration = max (iteration state - 1) 1
}

reducer state (Iteration it) = State {
    fractalType = fractalType state,
    iteration = it
}

reducer _ (NewType fractal) = State {
    fractalType = fractal,
    iteration = 1
}

reducer state IncreaseAngle = case fractalType state of
    KochFlakeType -> state
    PythagoreanTreeType angle isSymetric -> State {
        fractalType = PythagoreanTreeType (min (angle + 5) 180) isSymetric,
        iteration = iteration state
    }

reducer state DecreaseAngle = case fractalType state of
    KochFlakeType -> state
    PythagoreanTreeType angle isSymetric -> State {
        fractalType = PythagoreanTreeType (max (angle - 5) 0) isSymetric,
        iteration = iteration state
    }

reducer state ChangeSymetry = case fractalType state of
    KochFlakeType -> state
    PythagoreanTreeType angle isSymetric -> State {
        fractalType = PythagoreanTreeType angle (not isSymetric),
        iteration = iteration state
    }

prepareCanvas :: IO (UI.Canvas Action)
prepareCanvas = do
    font <- err $ fontFromFile fontPath

    let style = UI.Style {
        UI.buttonColor = white,
        UI.textColor = black,
        UI.textFont = font
    }

    let buttonCreator no = UI.createButton style (FloatRect {
        fleft = fromIntegral Main.drawerWidth,
        ftop = no * fromIntegral (Main.buttonHeight + Main.buttonsSpacing),
        fwidth = fromIntegral Main.menuWidth,
        fheight = fromIntegral Main.buttonHeight
    })

    buttonKochFlake <- buttonCreator 0 "Koch Flake" (NewType KochFlakeType)
    buttonPythagoreanTree <- buttonCreator 1 "Pythagorean Tree" (NewType (PythagoreanTreeType 90 False))
    buttonNextIteration <- buttonCreator 2 "+1 iteration" NextIteration
    buttonPreviousIteration <- buttonCreator 3 "-1 iteration" PreviousIteration
    buttonIncreaseAngle <- buttonCreator 4 "+5 degree" IncreaseAngle
    buttonDecreaseAngle <- buttonCreator 5 "-5 degree" DecreaseAngle
    buttonChangeSymetry <- buttonCreator 6 "change symetry" ChangeSymetry
    
    return UI.Canvas {
        UI.buttons = [
            buttonKochFlake,
            buttonPythagoreanTree,
            buttonNextIteration,
            buttonPreviousIteration,
            buttonIncreaseAngle,
            buttonDecreaseAngle,
            buttonChangeSymetry
        ]
    }


main :: IO ()
main = do
    putStr . show $ ()
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode (Main.drawerWidth + Main.menuWidth) Main.drawerHeight 32) "Fractals Drawer" [SFDefaultStyle] ctxSettings
    canvas <- prepareCanvas

    va <- drawFractal initialState
    loop wnd canvas initialState va
    destroy wnd


treeTransform :: Transform
treeTransform = 
        translation 0 height *
        rotationWithCenter 180 x_center 0 *
        translation (x_center - 50) 0 *
        scaling 100 100
    where 
        x_center = fromIntegral Main.drawerWidth / 2
        height = fromIntegral Main.drawerHeight


newLoop :: RenderWindow -> UI.Canvas Action -> State -> IO ()
newLoop wnd canvas state = do
    va <- drawFractal state
    loop wnd canvas state va


drawer :: RenderWindow -> UI.Canvas Action -> State -> VertexArray -> IO ()
drawer wnd canvas state va = do
    bounds <- getVABounds va
    let scale = min (fromIntegral Main.drawerWidth / fwidth bounds) (fromIntegral Main.drawerHeight / fheight bounds)
    clearRenderWindow wnd black
    drawVertexArray wnd va $ Just (renderStates { 
        transform =
            rotationWithCenter 180 x_center y_center *
            scalingWithCenter scale scale x_center y_center *
            translation 
                (x_center - (fleft bounds + fwidth bounds / 2)) 
                (y_center - (ftop bounds + fheight bounds / 2))
    })
    draw wnd canvas Nothing
    display wnd
    where 
        x_center = fromIntegral Main.drawerWidth / 2
        y_center = fromIntegral Main.drawerHeight / 2

loop :: RenderWindow -> UI.Canvas Action -> State -> VertexArray -> IO ()
loop wnd canvas state va = do
    drawer wnd canvas state va
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        Just (SFEvtMouseButtonPressed _ x y) -> do
            case UI.getMouseClicked (fromIntegral x) (fromIntegral y) canvas of
                Just action -> do
                    destroy va
                    newLoop wnd canvas $ reducer state action
                Nothing -> nextLoop 
        _ -> nextLoop
    where
        nextLoop = loop wnd canvas state va
