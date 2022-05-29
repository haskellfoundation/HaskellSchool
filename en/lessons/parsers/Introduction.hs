module Introduction where

import Text.ParserCombinators.ReadP

tuples :: ReadP [(String, String)]
tuples = many tuple

tuple :: ReadP (String, String)
tuple =
  between (char '(') (char ')') $ do
    left <- munch1 (/= ',')
    char ','
    skipSpaces
    right <- munch1 (/= ')')
    return (left, right)

example :: [([(String, String)], String)]
example = readP_to_S tuples "(abc,def)(gh,   ij)(k,l)"
