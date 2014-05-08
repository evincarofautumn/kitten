module Kitten.Parse.Layout
  ( insertBraces
  ) where

import Control.Applicative
import Control.Monad

import Kitten.Location
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Types
import Kitten.Util.List

insertBraces :: Parser [Located]
insertBraces = (concat <$> many unit) <* eof
  where

  bracket :: Token -> Token -> Parser [Located]
  bracket open close = do
    begin <- locatedMatch open
    inner <- concat <$> many unit
    end <- locatedMatch close
    return $ begin : inner ++ [end]

  unit :: Parser [Located]
  unit = unitWhere $ const True

  unitWhere :: (Located -> Bool) -> Parser [Located]
  unitWhere predicate
    = try (lookAhead $ locatedSatisfy predicate) *> choice
    [ bracket (TkBlockBegin NormalBlockHint) TkBlockEnd
    , bracket TkGroupBegin TkGroupEnd
    , bracket TkVectorBegin TkVectorEnd
    , layout
    , list <$> locatedSatisfy nonbracket
    ]

  nonbracket :: Located -> Bool
  nonbracket = not . (`elem` brackets) . locatedToken

  brackets :: [Token]
  brackets = blockBrackets ++
    [ TkGroupBegin
    , TkGroupEnd
    , TkVectorBegin
    , TkVectorEnd
    ]

  blockBrackets :: [Token]
  blockBrackets =
    [ TkBlockBegin NormalBlockHint
    , TkBlockEnd
    , TkLayout
    ]

  layout :: Parser [Located]
  layout = do
    Located
      { locatedLocation = colonLoc@Location
        {locationIndent = colonIndent}
      } <- locatedMatch TkLayout
    let
      inside (Located _ loc)
        = sourceColumn (locationStart loc) > colonIndent
    body <- concat <$> many (unitWhere inside)
    when (null body)
      $ fail "empty layout blocks are not allowed; use {} instead"
    return $ Located (TkBlockBegin LayoutBlockHint) colonLoc
      : body ++ [Located TkBlockEnd colonLoc]
