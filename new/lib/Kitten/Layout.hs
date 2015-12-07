module Kitten.Layout
  ( layout
  ) where

import Control.Applicative
import Kitten.Indent (Indent(..))
import Kitten.Informer (Informer(..))
import Kitten.Located (Located(..))
import Kitten.Monad (K)
import Kitten.Name (Qualifier(..))
import Kitten.Parser (Parser, parserMatch, tokenSatisfy)
import Kitten.Report (reportParseError)
import Kitten.Token (Token(..))
import Text.Parsec ((<?>))
import qualified Kitten.Layoutness as Layoutness
import qualified Kitten.Located as Located
import qualified Kitten.Origin as Origin
import qualified Text.Parsec as Parsec

layout :: FilePath -> [Located Token] -> K [Located Token]
layout path tokens
  = case Parsec.runParser insertBraces (Qualifier []) path tokens of
    Left parseError -> do
      report $ reportParseError parseError
      halt
    Right result -> return result

insertBraces :: Parser [Located Token]
insertBraces = (concat <$> many unit) <* Parsec.eof
  where

  unit :: Parser [Located Token]
  unit = unitWhere (const True)

  unitWhere :: (Located Token -> Bool) -> Parser [Located Token]
  unitWhere predicate
    = Parsec.try (Parsec.lookAhead (tokenSatisfy predicate)) *> Parsec.choice
      [ bracket (BlockBegin Layoutness.Nonlayout) BlockEnd
      , bracket GroupBegin GroupEnd
      , bracket VectorBegin VectorEnd
      , layoutBlock
      , (:[]) <$> tokenSatisfy nonbracket
      ]

  bracket :: Token -> Token -> Parser [Located Token]
  bracket open close = do
    begin <- parserMatch open
    inner <- concat <$> many unit
    end <- parserMatch close
    return (begin : inner ++ [end])

  nonbracket :: Located Token -> Bool
  nonbracket = not . (`elem` brackets) . Located.item

  brackets :: [Token]
  brackets = blockBrackets ++
    [ GroupBegin
    , GroupEnd
    , VectorBegin
    , VectorEnd
    ]

  blockBrackets :: [Token]
  blockBrackets =
    [ BlockBegin Layoutness.Nonlayout
    , BlockEnd
    , Layout
    ]

  layoutBlock :: Parser [Located Token]
  layoutBlock = do
    colon <- parserMatch Layout
    let
      colonOrigin = Located.origin colon
      colonIndent = Located.indent colon
      validFirst token = let
        column = Located.indent token
        in column > Indent (Just 1) && column >= colonIndent
    firstToken <- Parsec.lookAhead (tokenSatisfy validFirst)
      <?> "token indented no less than start of layout block"
    let
      firstOrigin = Origin.begin (Located.origin firstToken)
      inside token
        = Parsec.sourceColumn (Origin.begin (Located.origin token))
        >= Parsec.sourceColumn firstOrigin
    body <- concat <$> many (unitWhere inside)
    return $ At colonOrigin colonIndent (BlockBegin Layoutness.Layout)
      : body ++ [At colonOrigin colonIndent BlockEnd]
