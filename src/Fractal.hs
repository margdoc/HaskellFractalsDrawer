{-# LANGUAGE MultiParamTypeClasses #-}
module Fractal ( DoubleMonad(..), Fractal(..) ) where

class (Monad f) => DoubleMonad f where
    (>>>=) :: (f a, f b) -> (a -> (f a, f b), b -> (f a, f b)) -> (f a, f b)

class (DoubleMonad f) => Fractal f a b where
    gen :: (f a, f b) -> (a -> (f a, f b), b -> (f a, f b)) -> Int -> (f a, f b)
    gen fractal _ 1 = fractal
    gen fractal mappers n = gen (fractal >>>= mappers) mappers (n - 1)


mapToTuple :: [a] -> (a -> ([b], [c])) -> ([b], [c])
mapToTuple a mapper = (
        concatMap fst mapped,
        concatMap snd mapped
    )
    where
        mapped = fmap mapper a

instance DoubleMonad [] where
    (>>>=) (list_a, list_b) (mapper_a, mapper_b) = (
            fst mapped_a ++ fst mapped_b,
            snd mapped_a ++ snd mapped_b
        )
        where
            mapped_a = mapToTuple list_a mapper_a
            mapped_b = mapToTuple list_b mapper_b