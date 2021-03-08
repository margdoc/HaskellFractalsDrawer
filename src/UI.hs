{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module UI ( Button, Canvas(..), getMouseClicked, Style(..), createButton ) where
import SFML.Graphics ( FloatRect(..), floatRectContains, SFDrawable(draw), RectangleShape, Text, SFRenderTarget (drawRectangle, drawText), createRectangleShape, createText, SFTransformable (setPosition), setSize, Font, setTextString, setTextFont, Color, setColor, SFShape (setFillColor), setTextColor, setOrigin, SFBounded (getGlobalBounds, getLocalBounds), setTextCharacterSize )
import Data.Foldable ( find )
import SFML.SFResource ( SFResource(..) )
import SFML.Utils ( err )
import SFML.Window ( Vec2f(Vec2f) )


data Style = Style {
    buttonColor :: Color,
    textColor :: Color,
    textFont :: Font
}


class (SFDrawable a) => Widget a b | a -> b where
    mouseOver :: Float -> Float -> a -> Bool
    click :: Float -> Float -> a -> Maybe b


data Button a = Button {
    rect :: RectangleShape,
    bound :: FloatRect,
    text :: Text,
    onClick :: a
}


instance SFResource (Button a) where
    destroy button = do
        destroy $ rect button
        destroy $ text button


instance SFDrawable (Button a) where
  draw wnd button renderState = do
      drawRectangle wnd (rect button) renderState
      drawText wnd (text button) renderState


instance Widget (Button a) a where
    mouseOver x y button = floatRectContains x y (bound button)
    click  x y button
        | mouseOver x y button = Just (onClick button)
        | otherwise = Nothing


data Canvas a = Canvas {
    buttons :: [Button a]
}

instance SFResource (Canvas a) where
    destroy canvas = do
        mapM_ destroy $ buttons canvas

instance SFDrawable (Canvas a) where
    draw wnd canvas renderState = do
        mapM_ (($ renderState) . draw wnd) (buttons canvas)

getMouseClicked :: Float -> Float -> Canvas a -> Maybe a
getMouseClicked x y canvas =
    case find (mouseOver x y) (buttons canvas) of
        Just button -> click x y button
        Nothing -> Nothing

createButton :: Style -> FloatRect -> String -> a -> IO (Button a)
createButton style rect string onClick = do
    shape <- err createRectangleShape
    text <- err createText

    -- Rectangle Shape
    setPosition shape (Vec2f (fleft rect) (ftop rect))
    setSize shape (Vec2f (fwidth rect) (fheight rect))
    setFillColor shape $ buttonColor style

    -- Text
    setTextString text string
    setTextFont text $ textFont style
    setTextColor text $ textColor style
    setTextCharacterSize text 10
    textBounds <- getLocalBounds text
    let epsilon = 5 :: Float
    setTextCharacterSize text $ floor $ 10 * min ((fwidth rect - epsilon) / fwidth textBounds) ((fheight rect - epsilon) / fheight textBounds)
    textBounds <- getLocalBounds text
    setOrigin text (Vec2f (fwidth textBounds / 2) (fheight textBounds / 2))
    setPosition text (Vec2f (fleft rect + (fwidth rect / 2)) (ftop rect + (fheight rect / 2)))

    return Button {
        rect = shape,
        bound = rect,
        text = text,
        onClick = onClick
    }
