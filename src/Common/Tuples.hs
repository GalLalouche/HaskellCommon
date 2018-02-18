module Common.Tuples where

toTuple :: (a -> b) -> (a -> c) -> a -> (b, c)
toTuple f g x = (f x, g x)
