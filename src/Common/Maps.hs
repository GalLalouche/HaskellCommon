{-# LANGUAGE ScopedTypeVariables #-}

module Common.Maps where

import Common.Operators
import Data.Set (Set)
import Common.Lists (filterNot)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Map (Map, alter, empty, findWithDefault)
import Data.Foldable (foldr)
import Control.Monad (foldM)

-- if the value already exists, <> v to it; otherwise, just adds a v
upsert :: (Ord k, Monoid v) => k -> v -> Map k v -> Map k v
upsert k v = alter (fromMaybe mempty .> (<> v) .> Just) k

index :: (Ord key, Monoid value) => (a -> key) -> (a -> value) -> [a] -> Map key value
index keyF valueF = foldr upsertX empty where
  upsertX x = upsert (keyF x) (valueF x)

indexM :: (Ord key, Monoid value, Monad m) => (a -> m key) -> (a -> m value) -> [a] -> m (Map key value)
indexM keyF valueF = foldM aux empty where
  aux map x = do
    key <- keyF x
    value <- valueF x
    return $ upsert key value map

closure :: Ord a => a -> Map a [a] -> [a]
closure x map = aux [] S.empty $ S.fromList $ findWithDefault [] x map where
  pop :: Ord a => Set a -> (a, Set a)
  pop xs = let res = head $ S.toList xs
               popped = S.delete res xs
           in (res, popped)
  aux result visited q = let
      (h, res) = pop q
      newResult = h : result
      newVisited = S.insert h q
      newNeighbors = S.fromList $ findWithDefault [] h map |> filterNot (`S.member` newVisited)
      newQueue = S.union q newNeighbors
    in if S.null q then result else aux newResult newVisited newNeighbors
