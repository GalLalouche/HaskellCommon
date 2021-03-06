module Common.Foldables where
import Common.Operators
import Data.Foldable

notNull :: Foldable f => f a -> Bool
notNull = not . null

headOpt :: Foldable f => f a -> Maybe a
headOpt = nth 0

nth :: Foldable f => Int -> f a -> Maybe a
nth i = aux i . toList where
  aux _ [] = Nothing
  aux 0 (x:xs) = Just x
  aux n (_:xs) = aux (n - 1) xs
