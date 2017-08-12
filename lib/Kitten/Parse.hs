{-|
Module      : Kitten.Parse
Description : Parsing from tokens to terms
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.Parse
  ( generalName
  , fragment
  ) where

import Control.Applicative
import Control.Monad (guard, void)
import Data.List (find, findIndex, foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Kitten.Bracket (bracket)
import Kitten.DataConstructor (DataConstructor(DataConstructor))
import Kitten.Declaration (Declaration(Declaration))
import Kitten.Definition (Definition(Definition))
import Kitten.Element (Element)
import Kitten.Entry.Category (Category)
import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Fragment (Fragment(Fragment))
import Kitten.Informer (Informer(..))
import Kitten.Kind (Kind(..))
import Kitten.Layoutness (Layoutness(..))
import Kitten.Located (Located)
import Kitten.Metadata (Metadata(Metadata))
import Kitten.Monad (K)
import Kitten.Name
import Kitten.Origin (Origin, getOrigin)
import Kitten.Parser (Parser, getTokenOrigin, parserMatch, parserMatch_)
import Kitten.Phase (Phase(..))
import Kitten.Signature (Signature)
import Kitten.Synonym (Synonym(Synonym))
import Kitten.Term (Sweet(..), composed)
import Kitten.Token (Token)
import Kitten.Tokenize (tokenize)
import Kitten.TypeDefinition (TypeDefinition(TypeDefinition))
import Text.Parsec ((<?>))
import Text.Parsec.Pos (SourcePos)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.Declaration as Declaration
import qualified Kitten.Definition as Definition
import qualified Kitten.Desugar.Data as Data
import qualified Kitten.Element as Element
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Entry.Parent as Parent
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Located as Located
import qualified Kitten.Metadata as Metadata
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Token as Token
import qualified Kitten.TypeDefinition as TypeDefinition
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.Parsec as Parsec

-- | Parses a program fragment.

fragment
  :: Int
  -- ^ Initial source line (e.g. for REPL offset).
  -> FilePath
  -- ^ Source file path.
  -> [GeneralName]
  -- ^ List of permissions granted to @main@.
  -> Maybe Qualified
  -- ^ Override name of @main@.
  -> [Located (Token 'Nonlayout)]
  -- ^ Input tokens.
  -> K (Fragment 'Parsed)
  -- ^ Parsed program fragment.
fragment line path mainPermissions mainName tokens = let
  parsed = Parsec.runParser
    (fragmentParser mainPermissions mainName)
    Vocabulary.global path tokens
  in case parsed of
    Left parseError -> do
      report $ Report.parseError parseError
      halt
    Right result -> return (Data.desugar (insertMain result))

  where
    isMain = (== fromMaybe Definition.mainName mainName) . Definition.name
    insertMain f = case find isMain $ Fragment.definitions f of
      Just{} -> f
      Nothing -> f
        { Fragment.definitions = Definition.main mainPermissions mainName
          (SIdentity () (Origin.point path line 1))
          : Fragment.definitions f
        }

-- | Parses only a name.

generalName :: (Informer m) => Int -> FilePath -> Text -> m GeneralName
generalName line path text = do
  tokens <- tokenize line path text
  checkpoint
  bracketed <- bracket path tokens
  let parsed = Parsec.runParser nameParser Vocabulary.global path bracketed
  case parsed of
    Left parseError -> do
      report $ Report.parseError parseError
      halt
    Right (name, _) -> return name

fragmentParser
  :: [GeneralName] -> Maybe Qualified -> Parser (Fragment 'Parsed)
fragmentParser mainPermissions mainName
  = partitionElements mainPermissions mainName
    <$> elementsParser <* Parsec.eof

elementsParser :: Parser [Element 'Parsed]
elementsParser = concat <$> many (vocabularyParser <|> (:[]) <$> elementParser)

partitionElements
  :: [GeneralName]
  -> Maybe Qualified
  -> [Element 'Parsed]
  -> Fragment 'Parsed
partitionElements mainPermissions mainName = rev . foldr go mempty
  where

  rev :: Fragment 'Parsed -> Fragment 'Parsed
  rev f = Fragment
    { Fragment.declarations = reverse $ Fragment.declarations f
    , Fragment.definitions = reverse $ Fragment.definitions f
    , Fragment.metadata = reverse $ Fragment.metadata f
    , Fragment.synonyms = reverse $ Fragment.synonyms f
    , Fragment.types = reverse $ Fragment.types f
    }

  go :: Element 'Parsed -> Fragment 'Parsed -> Fragment 'Parsed
  go element acc = case element of
    Element.Declaration x -> acc
      { Fragment.declarations = x : Fragment.declarations acc }
    Element.Definition x -> acc
      { Fragment.definitions = x : Fragment.definitions acc }
    Element.Metadata x -> acc
      { Fragment.metadata = x : Fragment.metadata acc }
    Element.Synonym x -> acc
      { Fragment.synonyms = x : Fragment.synonyms acc }
    Element.TypeDefinition x -> acc
      { Fragment.types = x : Fragment.types acc }
    Element.Term x -> acc
      { Fragment.definitions
        = case findIndex
          ((== fromMaybe Definition.mainName mainName) . Definition.name)
          $ Fragment.definitions acc of
          Just index -> case splitAt index $ Fragment.definitions acc of
            (a, existing : b) -> a ++ existing { Definition.body
              = composeUnderLambda (Definition.body existing) x } : b
            _ -> error "cannot find main definition"
          Nothing -> Definition.main mainPermissions mainName x
            : Fragment.definitions acc
      }
      where

-- In top-level code, we want local variable bindings to remain in scope even
-- when separated by other top-level program elements, e.g.:
--
--     1 -> x;
--     define f (int -> int) { (+ 1) }
--     x say  // should work
--
-- As such, when composing top-level code, we extend the scope of lambdas to
-- include subsequent expressions.

      composeUnderLambda :: Sweet 'Parsed -> Sweet 'Parsed -> Sweet 'Parsed
      composeUnderLambda (SLambda _ origin vars body) term
        = SLambda () origin vars $ composeUnderLambda body term
      composeUnderLambda a b = SCompose () a b

vocabularyParser :: Parser [Element 'Parsed]
vocabularyParser = (<?> "vocabulary definition") $ do
  parserMatch_ Token.Vocab
  original@(Qualifier _ outer) <- Parsec.getState
  (vocabularyName, _) <- nameParser
  let
    (inner, name) = case vocabularyName of
      QualifiedName
        (Qualified (Qualifier _root qualifier) (Unqualified unqualified))
        -> (qualifier, unqualified)
      UnqualifiedName (Unqualified unqualified) -> ([], unqualified)
      LocalName{} -> error "local name should not appear as vocabulary name"
  Parsec.putState (Qualifier Absolute (outer ++ inner ++ [name]))
  Parsec.choice
    [ [] <$ parserMatchOperator ";"
    , do
      elements <- blockedParser elementsParser
      Parsec.putState original
      return elements
    ]

blockedParser :: Parser a -> Parser a
blockedParser = Parsec.between
  (parserMatch Token.BlockBegin)
  (parserMatch Token.BlockEnd)

groupedParser :: Parser a -> Parser a
groupedParser = Parsec.between
  (parserMatch Token.GroupBegin) (parserMatch Token.GroupEnd)

groupParser :: Parser (Sweet 'Parsed)
groupParser = do
  origin <- getTokenOrigin
  groupedParser
    $ SGroup () origin . composed origin
    <$> Parsec.many1 termParser

-- See note [Angle Brackets].

angledParser :: Parser a -> Parser a
angledParser = Parsec.between
  (parserMatchOperator "<" <|> parserMatch Token.AngleBegin)
  (parserMatchOperator ">" <|> parserMatch Token.AngleEnd)

bracketedParser :: Parser a -> Parser a
bracketedParser = Parsec.between
  (parserMatch Token.VectorBegin) (parserMatch Token.VectorEnd)

nameParser :: Parser (GeneralName, Operator.Fixity)
nameParser = (<?> "name") $ do
  global <- isJust <$> Parsec.optionMaybe
    (parserMatch Token.Ignore <* parserMatch Token.VocabLookup)
  parts <- Parsec.choice
    [ (,) Operator.Postfix <$> wordNameParser
    , (,) Operator.Infix <$> operatorNameParser
    ] `Parsec.sepBy1` parserMatch Token.VocabLookup
  return $ case parts of
    [(fixity, unqualified)]
      -> ((if global
        then QualifiedName . Qualified Vocabulary.global
        else UnqualifiedName)
      unqualified, fixity)
    _ -> let
      parts' = map ((\ (Unqualified part) -> part) . snd) parts
      qualifier = init parts'
      (fixity, unqualified) = last parts
      in
        ( QualifiedName
          (Qualified
            (Qualifier (if global then Absolute else Relative) qualifier)
            unqualified)
        , fixity
        )

unqualifiedNameParser :: Parser Unqualified
unqualifiedNameParser = (<?> "unqualified name")
  $ wordNameParser <|> operatorNameParser

wordNameParser :: Parser Unqualified
wordNameParser = (<?> "word name") $ parseOne
  $ \ token -> case Located.item token of
    Token.Word name -> Just name
    _ -> Nothing

operatorNameParser :: Parser Unqualified
operatorNameParser = (<?> "operator name") $ do
  -- Rihtlice hi sind Angle gehatene, for ðan ðe hi engla wlite habbað.
  angles <- many $ parseOne $ \ token -> case Located.item token of
    Token.AngleBegin -> Just "<"
    Token.AngleEnd -> Just ">"
    _ -> Nothing
  rest <- parseOne $ \ token -> case Located.item token of
    Token.Operator (Unqualified name) -> Just name
    _ -> Nothing
  return $ Unqualified $ Text.concat $ angles ++ [rest]

parseOne :: (Located (Token 'Nonlayout) -> Maybe a) -> Parser a
parseOne = Parsec.tokenPrim show advance
  where
  advance :: SourcePos -> t -> [Located (Token 'Nonlayout)] -> SourcePos
  advance _ _ (token : _) = Origin.begin $ Located.origin token
  advance sourcePos _ _ = sourcePos

elementParser :: Parser (Element 'Parsed)
elementParser = (<?> "top-level program element") $ Parsec.choice
  [ Element.Definition <$> Parsec.choice
    [ basicDefinitionParser
    , instanceParser
    , permissionParser
    ]
  , Element.Declaration <$> Parsec.choice
    [ traitParser
    , intrinsicParser
    ]
  , Element.Metadata <$> metadataParser
  , Element.Synonym <$> synonymParser
  , Element.TypeDefinition <$> typeDefinitionParser
  , do
    origin <- getTokenOrigin
    Element.Term . composed origin <$> Parsec.many1 termParser
  ]

synonymParser :: Parser Synonym
synonymParser = (<?> "synonym definition") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.Synonym
  from <- Qualified <$> Parsec.getState
    <*> unqualifiedNameParser
  (to, _) <- nameParser
  return $ Synonym from to origin

metadataParser :: Parser Metadata
metadataParser = (<?> "metadata block") $ do
  origin <- getTokenOrigin <* parserMatch Token.About
  -- FIXME: This only allows metadata to be defined for elements within the
  -- current vocabulary.
  name <- Qualified <$> Parsec.getState
    <*> Parsec.choice
      [ unqualifiedNameParser <?> "word identifier"
      , (parserMatch Token.Type *> wordNameParser)
        <?> "'type' and type identifier"
      ]
  fields <- blockedParser $ many $ (,)
    <$> (wordNameParser <?> "metadata key identifier")
    <*> (blockParser <?> "metadata value block")
  return Metadata
    { Metadata.fields = HashMap.fromList fields
    , Metadata.name = QualifiedName name
    , Metadata.origin = origin
    }

typeDefinitionParser :: Parser TypeDefinition
typeDefinitionParser = (<?> "type definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Type
  (name, fixity) <- qualifiedNameParser <?> "type definition name"
  case fixity of
    Operator.Infix -> Parsec.unexpected "type-level operator"
    _ -> pure ()
  parameters <- Parsec.option [] quantifierParser
  constructors <- Parsec.choice
    [ blockedParser $ many constructorParser
{-
    -- FIXME: If types and words are in the same namespace, a constructor can't
    -- have the same name as its type, so this convenience syntax doesn't work.
    , do
      constructorOrigin <- getTokenOrigin
      fields <- groupedParser $ constructorFieldsParser
      return $ (:[]) $ DataConstructor
        { DataConstructor.fields = fields
        , DataConstructor.name = unqualifiedName name
        , DataConstructor.origin = constructorOrigin
        }
-}
    ]
  return TypeDefinition
    { TypeDefinition.constructors = constructors
    , TypeDefinition.name = name
    , TypeDefinition.origin = origin
    , TypeDefinition.parameters = parameters
    }

constructorParser :: Parser DataConstructor
constructorParser = (<?> "constructor definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Case
  name <- wordNameParser <?> "constructor name"
  fields <- (<?> "constructor fields") $ Parsec.option []
    $ groupedParser constructorFieldsParser
  return DataConstructor
    { DataConstructor.fields = fields
    , DataConstructor.name = name
    , DataConstructor.origin = origin
    }

constructorFieldsParser :: Parser [Signature]
constructorFieldsParser = typeParser `Parsec.sepEndBy` commaParser

typeParser :: Parser Signature
typeParser = Parsec.try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  (effect, origin) <- Parsec.choice
    [ do
      leftVar <- stack
      leftTypes <- Parsec.option [] (commaParser *> left)
      origin <- arrow
      rightVar <- stack
      rightTypes <- Parsec.option [] (commaParser *> right)
      return
        ( Signature.StackFunction
          (Signature.Variable (UnqualifiedName leftVar) origin) leftTypes
          (Signature.Variable (UnqualifiedName rightVar) origin) rightTypes
        , origin)
    , do
      leftTypes <- left
      origin <- arrow
      rightTypes <- right
      return (Signature.Function leftTypes rightTypes, origin)
    ]
  permissions <- (<?> "permission labels")
    $ many $ parserMatchOperator "+" *> (fst <$> nameParser)
  return (effect permissions origin)
  where

  stack :: Parser Unqualified
  stack = Parsec.try $ wordNameParser <* parserMatch Token.Ellipsis

  left, right :: Parser [Signature]
  left = basicTypeParser `Parsec.sepEndBy` commaParser
  right = typeParser `Parsec.sepEndBy` commaParser

  arrow :: Parser Origin
  arrow = getTokenOrigin <* parserMatch Token.Arrow

commaParser :: Parser ()
commaParser = void $ parserMatch Token.Comma

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  prefix <- Parsec.choice
    [ quantifiedParser $ groupedParser typeParser
    , Parsec.try $ do
      origin <- getTokenOrigin
      (name, fixity) <- nameParser
      -- Must be a word, not an operator, but may be qualified.
      guard $ fixity == Operator.Postfix
      return $ Signature.Variable name origin
    , groupedParser typeParser
    ]
  let apply a b = Signature.Application a b $ Signature.origin prefix
  mSuffix <- Parsec.optionMaybe $ fmap concat
    $ Parsec.many1 $ typeListParser basicTypeParser
  return $ case mSuffix of
    Just suffix -> foldl' apply prefix suffix
    Nothing -> prefix

quantifierParser :: Parser [Parameter]
quantifierParser = typeListParser var
  where

  var :: Parser Parameter
  var = do
    origin <- getTokenOrigin
    Parsec.choice
      [ (\ unqualified -> Parameter origin unqualified Permission)
        <$> (parserMatchOperator "+" *> wordNameParser)
      , do
        name <- wordNameParser
        Parameter origin name
          <$> Parsec.option Value (Stack <$ parserMatch Token.Ellipsis)
      ]

typeListParser :: Parser a -> Parser [a]
typeListParser element = angledParser
  $ element `Parsec.sepEndBy1` commaParser

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getTokenOrigin
  Signature.Quantified <$> quantifierParser <*> thing <*> pure origin

traitParser :: Parser Declaration
traitParser = (<?> "intrinsic declaration")
  $ declarationParser Token.Trait Declaration.Trait

intrinsicParser :: Parser Declaration
intrinsicParser = (<?> "intrinsic declaration")
  $ declarationParser Token.Intrinsic Declaration.Intrinsic

declarationParser
  :: Token 'Nonlayout
  -> Declaration.Category
  -> Parser Declaration
declarationParser keyword category = do
  origin <- getTokenOrigin <* parserMatch keyword
  suffix <- unqualifiedNameParser <?> "declaration name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  signature <- signatureParser
  return Declaration
    { Declaration.category = category
    , Declaration.name = name
    , Declaration.origin = origin
    , Declaration.signature = signature
    }

basicDefinitionParser :: Parser (Definition 'Parsed)
basicDefinitionParser = (<?> "word definition")
  $ definitionParser Token.Define Category.Word

instanceParser :: Parser (Definition 'Parsed)
instanceParser = (<?> "instance definition")
  $ definitionParser Token.Instance Category.Instance

permissionParser :: Parser (Definition 'Parsed)
permissionParser = (<?> "permission definition")
  $ definitionParser Token.Permission Category.Permission

-- | Unqualified or partially qualified name, implicitly qualified by the
-- current vocabulary, or fully qualified (global) name.
qualifiedNameParser :: Parser (Qualified, Operator.Fixity)
qualifiedNameParser = (<?> "optionally qualified name") $ do
  (suffix, fixity) <- nameParser
  name <- case suffix of
    QualifiedName qualified@(Qualified (Qualifier root parts) unqualified)
      -> case root of
        -- Fully qualified name: return it as-is.
        Absolute -> pure qualified
        -- Partially qualified name: add current vocab prefix to qualifier.
        Relative -> do
          Qualifier root' prefixParts <- Parsec.getState
          pure (Qualified (Qualifier root' (prefixParts ++ parts)) unqualified)
    -- Unqualified name: use current vocab prefix as qualifier.
    UnqualifiedName unqualified
      -> Qualified <$> Parsec.getState <*> pure unqualified
    _ -> error "name parser should only return qualified or unqualified name"
  pure (name, fixity)

definitionParser :: Token 'Nonlayout -> Category -> Parser (Definition 'Parsed)
definitionParser keyword category = do
  origin <- getTokenOrigin <* parserMatch keyword
  (name, fixity) <- qualifiedNameParser <?> "definition name"
  signature <- signatureParser
  body <- blockLikeParser <?> "definition body"
  return Definition
    { Definition.body = body
    , Definition.category = category
    , Definition.fixity = fixity
    , Definition.inferSignature = False
    , Definition.merge = Merge.Deny
    , Definition.name = name
    , Definition.origin = origin
    -- HACK: Should be passed in from outside?
    , Definition.parent = case keyword of
      Token.Instance -> Just $ Parent.Type name
      _ -> Nothing
    , Definition.signature = signature
    }

signatureParser :: Parser Signature
signatureParser = quantifiedParser signature <|> signature <?> "type signature"
  where signature = groupedParser functionTypeParser

blockParser :: Parser (Sweet 'Parsed)
blockParser = (blockedParser blockContentsParser <|> reference)
  <?> "block or reference"
  where
  reference = parserMatch_ Token.Reference *> Parsec.choice
    [ do
      origin <- getTokenOrigin
      SWord () origin Operator.Postfix
        <$> (fst <$> nameParser) <*> pure []
    , termParser
    ]

blockContentsParser :: Parser (Sweet 'Parsed)
blockContentsParser = do
  origin <- getTokenOrigin
  terms <- many termParser
  let origin' = case terms of { x : _ -> getOrigin x; _ -> origin }
  return $ composed origin' terms

termParser :: Parser (Sweet 'Parsed)
termParser = (<?> "expression") $ do
  origin <- getTokenOrigin
  Parsec.choice
    [ Parsec.try literalParser
    , do
      (name, fixity) <- nameParser
      return (SWord () origin fixity name [])
    , Parsec.try sectionParser
    , Parsec.try groupParser <?> "parenthesized expression"
    , vectorParser
    , lambdaParser
    , matchParser
    , ifParser
    , doParser
    , blockValue
    , withParser
    , asParser
    ]
  where

    literalParser :: Parser (Sweet 'Parsed)
    literalParser = parseOne toLiteral
      where

        toLiteral :: Located (Token 'Nonlayout) -> Maybe (Sweet 'Parsed)
        toLiteral token = case Located.item token of
          -- TODO: Differentiate types of character and text literals
          Token.Character x -> Just $ SCharacter () origin $ Text.singleton x
          Token.Float x -> Just $ SFloat () origin x
          Token.Integer x -> Just $ SInteger () origin x
          Token.Text x -> Just $ SText () origin x
          _ -> Nothing
          where

          origin :: Origin
          origin = Located.origin token

    sectionParser :: Parser (Sweet 'Parsed)
    sectionParser = (<?> "operator section") $ groupedParser $ Parsec.choice
      [ do
        origin <- getTokenOrigin
        function <- operatorNameParser
        Parsec.choice
          [ do
            operandOrigin <- getTokenOrigin
            operand <- Parsec.many1 termParser
            pure $ SSection () origin (UnqualifiedName function) False
              (composed operandOrigin operand) []
          , pure $ SWord () origin Operator.Postfix
            (UnqualifiedName function) []
          ]
      , do
        operandOrigin <- getTokenOrigin
        operand <- Parsec.many1
          $ Parsec.notFollowedBy operatorNameParser *> termParser
        origin <- getTokenOrigin
        function <- operatorNameParser
        return $ SSection () origin (UnqualifiedName function) True
          (composed operandOrigin operand) []
      ]

    vectorParser :: Parser (Sweet 'Parsed)
    vectorParser = (<?> "vector literal") $ do
      vectorOrigin <- getTokenOrigin
      elements <- bracketedParser
        $ (composed vectorOrigin <$> Parsec.many1 termParser)
        `Parsec.sepEndBy` commaParser
      return $ SList () vectorOrigin elements

    lambdaParser :: Parser (Sweet 'Parsed)
    lambdaParser = (<?> "variable introduction") $ do
      arrowOrigin <- getTokenOrigin
      names <- parserMatch Token.Arrow *> lambdaNamesParser
      let lambda = SLambda () arrowOrigin names
      Parsec.choice
        [ parserMatchOperator ";" *> do
          body <- blockContentsParser
          return $ lambda body
        , do
          body <- blockParser
          return $ SQuotation () (getOrigin body) $ lambda body
        ]

    matchParser :: Parser (Sweet 'Parsed)
    matchParser = (<?> "match") $ do
      matchOrigin <- getTokenOrigin <* parserMatch Token.Match
      mScrutinee <- Parsec.optionMaybe groupParser <?> "scrutinee"
      (cases, mElse) <- do
        cases' <- many $ (<?> "case") $ parserMatch Token.Case *> do
          origin <- getTokenOrigin
          (name, _) <- nameParser
          body <- blockLikeParser
          return (origin, name, body)
        mElse' <- Parsec.optionMaybe
          $ parserMatch Token.Else *> blockParser
        return (cases', mElse')
      return $ SMatch () matchOrigin mScrutinee cases mElse

    ifParser :: Parser (Sweet 'Parsed)
    ifParser = (<?> "if-else expression") $ do
      ifOrigin <- getTokenOrigin <* parserMatch Token.If
      mCondition <- Parsec.optionMaybe groupParser <?> "condition"
      ifBody <- blockParser
      elifs <- many $ do
        origin <- getTokenOrigin <* parserMatch Token.Elif
        condition <- groupParser <?> "condition"
        body <- blockParser
        return (origin, condition, body)
      mElse <- Parsec.optionMaybe $ parserMatch Token.Else *> blockParser
      return $ SIf () ifOrigin mCondition ifBody elifs mElse

    doParser :: Parser (Sweet 'Parsed)
    doParser = (<?> "do expression") $ do
      doOrigin <- getTokenOrigin <* parserMatch Token.Do
      term <- groupParser <?> "parenthesized expression"
      Parsec.choice
        -- do (f) { x y z } => { x y z } f
        [ do
          body <- blockLikeParser
          return $ SDo () doOrigin term
            $ SQuotation () (getOrigin body) body
        -- do (f) [x, y, z] => [x, y, z] f
        , do
          body <- vectorParser
          return $ SDo () doOrigin term body
        -- TODO: Support unboxed arrays and quotations.
        ]

    blockValue :: Parser (Sweet 'Parsed)
    blockValue = (<?> "quotation")
      $ SQuotation () <$> getTokenOrigin <*> blockParser

    asParser :: Parser (Sweet 'Parsed)
    asParser = (<?> "'as' expression") $ do
      origin <- getTokenOrigin <* parserMatch_ Token.As
      signatures <- groupedParser $ basicTypeParser `Parsec.sepEndBy` commaParser
      return $ SAs () origin signatures

    -- A 'with' term is parsed as a coercion followed by a call.
    withParser :: Parser (Sweet 'Parsed)
    withParser = (<?> "'with' expression") $ do
      origin <- getTokenOrigin <* parserMatch_ Token.With
      permits <- groupedParser $ Parsec.many1 permitParser
      return $ SWith () origin permits
      where

      permitParser :: Parser Term.Permit
      permitParser = Term.Permit <$> Parsec.choice
        [ True <$ parserMatchOperator "+"
        , False <$ parserMatchOperator "-"
        ] <*> (UnqualifiedName <$> wordNameParser)

parserMatchOperator :: Text -> Parser (Located (Token 'Nonlayout))
parserMatchOperator = parserMatch . Token.Operator . Unqualified

lambdaNamesParser :: Parser [(Origin, Maybe Unqualified, ())]
lambdaNamesParser = lambdaName `Parsec.sepEndBy1` commaParser
  where
  lambdaName = do
    origin <- getTokenOrigin
    name <- Just <$> wordNameParser <|> Nothing <$ parserMatch Token.Ignore
    return (origin, name, ())

blockLikeParser :: Parser (Sweet 'Parsed)
blockLikeParser = Parsec.choice
  [ blockParser
  , parserMatch Token.Arrow *> do
    names <- lambdaNamesParser
    origin <- getTokenOrigin
    body <- blockParser
    return $ SLambda () origin names body
  ]
