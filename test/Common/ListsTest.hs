{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.ListsTest where

import Common.Lists
import Test.Tasty.QuickCheck
import Data.List
import Data.Traversable
import Data.Foldable (toList)

prop_single (x :: Int) = Just x == single [x]
prop_single_error (xs :: [Int]) = (length xs /= 1) ==> Nothing === single xs
prop_nth (xs :: [Int]) x = not (x >= length xs || x < 0) ==> nth x xs == xs !! x
prop_dropr (xs :: [Int]) x = not (x >= length xs || x < 0) ==> length xs - x == length (dropr x xs)
prop_dropr_prefix (xs :: [Int]) x = not (x >= length xs || x < 0) ==> dropr x xs `isPrefixOf` xs
prop_split (x :: Int) y z = not (z == y || x == y) ==> split y [x, y, z] == [[x], [z]]
prop_filterNot (xs :: [Int]) = length f + length notF == length xs where
   f = filter odd xs
   notF = filterNot odd xs
prop_filterNot_intersect (xs :: [Int]) = null $ intersect f notF where
   f = filter odd xs
   notF = filterNot odd xs
prop_strip (xs :: [Int]) x = case strip x xs of
   (y:ys) -> y /= x
   [] -> True
prop_strip_suffix (xs :: [Int]) x = strip x xs `isSuffixOf` xs
 -- all lists, except possibly the last one, have a length of exactly x.
 -- The last one can have a length of less than x.
prop_grouped (xs :: [Int]) x = x > 0 && x < length xs ==> length last <= x && all ((==x) . length) heads where
   (last, heads) = let r = reverse $ grouped x xs in (head r, tail r)
prop_grouped_concat (xs :: [Int]) x = x > 0 ==> xs == concat (grouped x xs)
prop_fromList (xs :: [Int]) = xs == fromList xs
prop_withIndex (xs :: [Int]) = withIndex xs == zip xs [0..]

return []
runTests = $quickCheckAll
