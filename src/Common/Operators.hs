module Common.Operators where

import Control.Monad ((>=>), (<=<))

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 4 |>

(|>>) :: a -> (b -> a -> c) -> b -> c
x |>> f = \b -> f b x
infixl 4 |>>

(|>>>) :: a -> (b -> c -> a -> d) -> b -> c -> d
x |>>> f = \b c -> f b c x
infixl 4 |>>>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 4 .>

(..>) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(..>) f g = f .> (g .)
infixl 4 ..>

(...>) :: (a -> b -> c -> d) -> (d -> e) -> (a -> b -> c -> e)
(...>) f g = f ..> (g .)
infixl 4 ...>

fproduct :: Functor f => (a -> b) -> f a -> f (a, b)
fproduct f = fmap (\x -> (x, f x))

(-$>) :: Functor f => (a -> b) -> f a -> f (a, b)
(-$>) = fproduct
infixl 4 -$>

(<$-) :: Functor f => f a -> (a -> b) -> f (a, b)
(<$-) = flip (-$>)
infixl 4 <$-

-- fmap equivalent of <**>
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

infixl 4 <$$> -- Same priority as <**>

-- fmap equivalent of >=> and <=<
(>$>) :: Monad m => (a -> m b) -> (b -> c) -> (a -> m c)
f >$> g = f >=> (return . g)
infixr 1 >$> -- same priority as >=>

(<$<) :: Monad m => (b -> c) -> (a -> m b) -> (a -> m c)
g <$< f = (return . g) <=< f

infixr 1 <$< -- same priority as <=<
