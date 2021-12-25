module Parsec where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser emptyDef

numParser :: Parsec String Integer Integer
numParser = do
  n <- integer tokenParser
  previous <- getState
  guard $ n > previous
  putState n
  return n

increasingNumbersParser :: Parsec String Integer [Integer]
increasingNumbersParser =
  numParser `sepBy` spaces

increasingNumbers :: String -> Either ParseError [Integer]
increasingNumbers = runParser increasingNumbersParser (-1) ""
