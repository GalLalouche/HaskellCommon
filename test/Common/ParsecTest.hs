module Common.ParsecTest where

import Common.Parsec

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Pos as P
import Text.Parsec.Prim(parserFail)

import Control.Applicative
import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

successfulParse :: Show a => Parser a -> String -> a
successfulParse parser str = case P.parse parser "(test string)" str of
  (Right x) -> x
  (Left x) -> error $ "Expected successful parse but was " ++ show x

isFailedParse  :: Parser a -> String -> IO ()
isFailedParse parser str = assertBool "Parser was supposed to fail but didn't" $ isLeft $ P.parse parser "(test string)" str

undefinedParser :: Parser ()
undefinedParser = parserFail "undefined"

-- Really??
instance Show P.Message where
  show (P.SysUnExpect s) = "SysUnExpect " ++ s
  show (P.UnExpect    s) = "UnExpect " ++ s
  show (P.Expect      s) = "Expect " ++ s
  show (P.Message     s) = "Message " ++ s

test_parseEither = testGroup "parseEither" [
    testCase "Left parser succeeds" $
      successfulParse (parseEither P.digit undefinedParser) "4" @?= Left '4'
  , testCase "Left parser fails, right succeeds" $
      successfulParse (parseEither P.digit P.letter) "a" @?= Right 'a'
  , testCase "Tries left first" $
      successfulParse (parseEither P.digit P.digit) "4" @?= Left '4'
  , testCase "None succeed" $ let
      error = case P.parse (parseEither P.letter P.digit) "test string" "." of
        (Left e) -> e
      msgs = [P.SysUnExpect "\".\"", P.SysUnExpect "\".\"", P.Expect "letter", P.Expect "digit"]
      pos = P.newPos "test string" 1 1
    in do
      msgs @=? P.errorMessages error
      pos @=? P.errorPos error
  ]

test_parseAtLeast = testGroup "parseAtLeast" [
    testGroup "n = 0" [
      testCase "nothing up ahead" $
        successfulParse (parseAtLeast 0 P.digit) "foo" @?= ""
    , testCase "something up ahead" $
        successfulParse (parseAtLeast 0 P.letter) "foo" @?= "foo"
    ]
  , testGroup "n > 0" [
      testCase "exactly" $
        successfulParse (parseAtLeast 3 P.letter) "foo" @?= "foo"
    , testCase "less than actual" $
        successfulParse (parseAtLeast 3 P.letter) "foobar" @?= "foobar"
    , testCase "more than actual" $
        isFailedParse (parseAtLeast 4 P.letter) "foo123"
    ]
  ]

test_manyTill1 = testGroup "manyTill1" [
    testCase "has at least one" $
      successfulParse (manyTill1 P.letter P.digit) "foo123" @?= "foo"
  , testCase "has none" $
      isFailedParse (manyTill1 P.letter P.digit) "123"
  ]
