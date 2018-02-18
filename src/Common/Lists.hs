module Common.Lists where

import Common.Operators
import Control.Monad
import Data.Foldable (toList, fold, foldr)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Prelude as P hiding (foldl)
import Data.Map (Map, alter)
import qualified Data.Map as M

-- drop n elements from the end of the list
dropr :: Int -> [a] -> [a]
dropr n = reverse .> drop n .> reverse

-- splits a list at a given delimeter; the returned list of lists does not include the delimeter
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = let
    head = takeWhile (/= x) xs
    tail = split x (niceTail (dropWhile (/= x) xs)) where niceTail = fold . tailOpt
  in head : tail

nth :: Int -> [a] -> a
nth = flip (!!)

-- returns a list of lists of size n, last list is of size <= n; negative number will return the entire list
grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = let (h, t) = splitAux n xs in h : grouped n t where
  splitAux _ [] = ([], [])
  splitAux 0 xs = ([], xs)
  splitAux n (x:xs) = let helper = splitAux (n - 1) xs in (x : fst helper, snd helper)

-- should go in ApplicativePlus
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot = filter . not' where not' = (.) not

strip :: Eq a => a -> [a] -> [a]
strip _ [] = []
strip e (x:xs) = if x == e then strip e xs else x : xs

single :: [a] -> Maybe a
single [a] = Just a
single _ = Nothing

tailOpt :: [a] -> Maybe [a]
tailOpt [] = Nothing
tailOpt (x:xs) = Just xs

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = map (\x -> (x, f x))

-- An alternative actually suffices, but since Alternative isn't a superclass of MonadPlus, and
-- since MonadPlus is actually more common than Alternative, it just accepts an MonadPlus
fromList :: MonadPlus m => [a] -> m a
fromList = P.foldr (mplus . return) mzero

withIndex :: (Foldable m, MonadPlus m) => m a -> m (a, Int)
withIndex xs = toList xs |> flip zip [0..] |> fromList

update :: Int -> a -> [a] -> [a]
update 0 e (_:xs) = e : xs
update n e (x:xs) = x : update (n - 1) e xs
update _ _ _ = error "Index out of bounds for update"

windows :: Int -> [a] -> [[a]]
windows 0 _ = []
windows n xs = aux xs [] |> dropr (n - 1) where
  aux :: [a] -> [[a]] -> [[a]]
  aux [] res = reverse res
  aux xs res = aux (tail xs) (take n xs : res)

pairWindows :: [a] -> [(a, a)]
pairWindows = windows 2 .> map (\xs -> (xs !! 0, xs !! 1))
