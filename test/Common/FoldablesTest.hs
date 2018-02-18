{-# LANGUAGE TemplateHaskell #-}
module Common.FoldablesTest where

import Common.Foldables
import Test.QuickCheck

prop_notNull :: [Int] -> Bool
prop_notNull xs = notNull xs == not (null xs)

prop_headOption :: [Int] -> Bool
prop_headOption xs = headOpt xs == (if null xs then Nothing else Just $ head xs)

prop_nth_negative :: Int -> [Int] -> Property
prop_nth_negative x xs = x < 0 ==> Nothing == nth x xs
prop_nth_too_large :: Int -> [Int] -> Property
prop_nth_too_large x xs = x > length xs ==> Nothing == nth x xs
prop_nth :: Int -> [Int] -> Property
prop_nth x xs = x > 0 && x < length xs ==> Just (xs !! x) == nth x xs

return []
main = $quickCheckAll
