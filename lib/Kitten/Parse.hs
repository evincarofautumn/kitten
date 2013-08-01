module Kitten.Parse
  ( parse
  ) where

import Control.Applicative hiding (some)
import Control.Monad

import qualified Text.Parsec as Parsec

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Location
import Kitten.Parsec
import Kitten.Parse.Element
import Kitten.Parse.Layout
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parse.Type
import Kitten.Term
import Kitten.Token (Located(..), Token)
import Kitten.Util.List

import qualified Kitten.Token as Token

parse
  :: String
  -> [Located]
  -> Either ParseError (Fragment Value Term)
parse name
  = Parsec.parse insertBraces name
  >=> Parsec.parse fragment name

fragment :: Parser (Fragment Value Term)
fragment = partitionElements <$> (many element <* eof)

element :: Parser Element
element = choice
  [ DefElement <$> def
  , ImportElement <$> import_
  , TermElement <$> term
  ]

def :: Parser (Def Value)
def = (<?> "definition") . locate $ do
  void (match Token.Def)
  name <- functionName
  anno <- optionMaybe (grouped signature)
  body <- term
  return $ \ loc -> Def
    { defName = name
    , defTerm = case body of
      Push function@Function{} _ -> function
      _ -> Function [body] loc
    , defAnno = anno
    , defLocation = loc
    }

import_ :: Parser Import
import_ = (<?> "import") . locate $ do
  void (match Token.Import)
  name <- bigWord
  return $ \ loc -> Import
    { importName = name
    , importLocation = loc
    }

term :: Parser Term
term = locate $ choice
  [ try $ Push <$> value
  , Call <$> functionName
  , VectorTerm <$> vector
  , try group
  , pair <$> tuple
  , mapOne toBuiltin <?> "builtin"
  , lambda
  , if_
  , choiceTerm
  , optionTerm
  ]
  where

  group :: Parser (Location -> Term)
  group = (<?> "group") $ Group <$> grouped (many1 term)

  branchList :: Parser [Term]
  branchList = block <|> list <$> term

  branch :: Parser Term
  branch = locate $ Compose <$> branchList

  elseBranch :: Parser Term
  elseBranch = locate $ Compose
    <$> (option [] (match Token.Else *> branchList))

  if_ :: Parser (Location -> Term)
  if_ = (<?> "if") $ do
    void (match Token.If)
    condition <- term
    then_ <- branch
    else_ <- elseBranch
    return $ \ loc -> Compose [condition, If then_ else_ loc] loc

  choiceTerm :: Parser (Location -> Term)
  choiceTerm = (<?> "choice") $ do
    void (match Token.Choice)
    condition <- term
    left <- branch
    right <- elseBranch
    return $ \ loc -> Compose
      [condition, ChoiceTerm left right loc]
      loc

  optionTerm :: Parser (Location -> Term)
  optionTerm = (<?> "option") $ do
    void (match Token.Option)
    condition <- term
    some <- branch
    none <- elseBranch
    return $ \ loc -> Compose
      [condition, OptionTerm some none loc]
      loc

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> locate (Compose <$> many term)
    , do
      names <- blocked (many littleWord)
      terms <- many term
      return $ \ loc -> foldr
        (\ lambdaName lambdaTerms -> Lambda lambdaName lambdaTerms loc)
        (Compose terms loc)
        (reverse names)
    ]

  pair :: [Term] -> Location -> Term
  pair values loc
    = foldr (\ x y -> PairTerm x y loc) (Push (Unit loc) loc) values

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser [Term]
  tuple = grouped
    (locate (Compose <$> many1 term) `sepEndBy1` match Token.Comma)
    <?> "tuple"

  vector :: Parser [Term]
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose <$> many1 term) `sepEndBy` match Token.Comma)
    <?> "vector"

value :: Parser Value
value = locate $ choice
  [ mapOne toLiteral <?> "literal"
  , Function <$> block <?> "function"
  , unit
  ]
  where

  toLiteral :: Token -> Maybe (Location -> Value)
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Char x) = Just $ Char x
  toLiteral (Token.Float x) = Just $ Float x
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Text x) = Just $ \ loc
    -> Vector (map (`Char` loc) x) loc
  toLiteral _ = Nothing

  unit :: Parser (Location -> Value)
  unit = Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser [Term]
block = blocked (many term) <?> "block"
