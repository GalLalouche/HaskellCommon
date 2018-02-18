module Common.Bools where

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x

or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or f g x = f x || g x
