{-# LANGUAGE MultiParamTypeClasses #-}
module BasicFractal (BasicFractal(..)) where

class (Monad f) => BasicFractal f a where
    gen :: f a -> (a -> f a) -> Int -> f a
    gen fractal _ 1 = fractal
    gen fractal f n = gen (fractal >>= f) f (n - 1)
