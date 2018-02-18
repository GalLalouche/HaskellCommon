module Common.Parsec where

import Common.Operators
import Text.Parsec((<?>))
import Data.Char(isSpace)
import Data.Functor
import qualified Text.Parsec as P
import Control.Applicative

type Parser a = P.Parsec String () a

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = (Left <$> pa) <|> (Right <$> pb)

parseAtLeast :: Int -> Parser a -> Parser [a]
parseAtLeast n p = (++) <$> P.count n p <*> P.many p

manyTill1 :: Parser a -> Parser end -> Parser [a]
manyTill1 p end = (:) <$> p <*> P.manyTill p end

nonSpace :: Parser Char
nonSpace = P.satisfy (not . isSpace) <?> "nonSpace"

nonEndLine :: Parser Char
nonEndLine = P.noneOf ['\n', '\r', '\v'] <?> "nonEndLine"

lineSpace :: Parser Char
lineSpace = P.oneOf [' ', '\t'] <?> "line-space"

lineSpaces :: Parser ()
lineSpaces = P.skipMany (P.oneOf [' ', '\t']) <?> "line-spaces"

parseLine :: Parser String
parseLine = P.many nonEndLine <* P.endOfLine

parseLines :: Parser [String]
parseLines = P.many $ P.many nonEndLine <* P.endOfLine

emptyLine :: Parser ()
emptyLine = P.string "\n" $> ()
