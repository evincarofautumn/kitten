module Kitten.Parse
  ( parse
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V
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
import Kitten.TypeDef
import Kitten.Token (Located(..), Token)
import Kitten.Util.Parsec

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
  , TypeElement <$> type_
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
      _ -> Function (V.singleton body) loc
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
  , to
  , from
  ]
  where

  group :: Parser (Location -> Term)
  group = (<?> "group") $ Group <$> grouped (many1V term)

  branchList :: Parser (Vector Term)
  branchList = block <|> V.singleton <$> term

  branch :: Parser Term
  branch = locate $ Compose <$> branchList

  elseBranch :: Parser Term
  elseBranch = locate $ Compose
    <$> option V.empty (match Token.Else *> branchList)

  if_ :: Parser (Location -> Term)
  if_ = (<?> "if") $ do
    void (match Token.If)
    condition <- term
    then_ <- branch
    else_ <- elseBranch
    return $ \ loc -> Compose
      (V.fromList [condition, If then_ else_ loc])
      loc

  choiceTerm :: Parser (Location -> Term)
  choiceTerm = (<?> "choice") $ do
    void (match Token.Choice)
    condition <- term
    left <- branch
    right <- elseBranch
    return $ \ loc -> Compose
      (V.fromList [condition, ChoiceTerm left right loc])
      loc

  optionTerm :: Parser (Location -> Term)
  optionTerm = (<?> "option") $ do
    void (match Token.Option)
    condition <- term
    some <- branch
    none <- elseBranch
    return $ \ loc -> Compose
      (V.fromList [condition, OptionTerm some none loc])
      loc

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> locate (Compose <$> manyV term)
    , do
      names <- blocked (many littleWord)
      terms <- manyV term
      return $ \ loc -> foldr
        (\ lambdaName lambdaTerms -> Lambda lambdaName lambdaTerms loc)
        (Compose terms loc)
        (reverse names)
    ]

  pair :: Vector Term -> Location -> Term
  pair values loc = V.foldr (\ x y -> PairTerm x y loc)
    (Push (Unit loc) loc) values

  to :: Parser (Location -> Term)
  to = To <$> (match Token.To *> bigWord)

  from :: Parser (Location -> Term)
  from = From <$> (match Token.From *> bigWord)

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser (Vector Term)
  tuple = grouped
    (locate (Compose <$> many1V term) `sepEndBy1V` match Token.Comma)
    <?> "tuple"

  vector :: Parser (Vector Term)
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose <$> many1V term) `sepEndByV` match Token.Comma)
    <?> "vector"

type_ :: Parser TypeDef
type_ = (<?> "type definition") . locate $ do
  void (match Token.Type)
  name <- bigWord
  anno <- typeDefType
  return $ \ loc -> TypeDef
    { typeDefName = name
    , typeDefAnno = anno
    , typeDefLocation = loc
    }

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
    -> Vector (V.fromList (map (`Char` loc) (T.unpack x))) loc
  toLiteral _ = Nothing

  unit :: Parser (Location -> Value)
  unit = Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser (Vector Term)
block = blocked (manyV term) <?> "block"
