module Parse 
  ( Parse.parse
  ) where

import Error
import Program
import Term
import qualified Text
import qualified Token
import Tokenize
import Utils

import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad.Identity
import Data.Char
import Text.Parsec.Error as Parsec
import Text.Parsec.Pos as Parsec
import Text.Parsec.Prim as Parsec hiding (token, tokens)
import Text.Parsec.Combinator as Parsec

type TokenParser a = ParsecT [Token.Located] () Identity a

parse
  :: String
  -> String
  -> Either ParseError Program
parse source name
  = case tokenize source name of
      Right source' -> parse' program source' name
      Left err -> Left err

parse'
  :: TokenParser a
  -> [Token.Located]
  -> String
  -> Either ParseError a
parse' parser source name
  = Parsec.runParser parser () name source

program :: TokenParser Program
program = Program <$> program'

program' :: TokenParser [Term]
program' = many term <* eof

term :: TokenParser Term
term = choice (try definition : normalTerm) <?> "term"

normalTerm :: [TokenParser Term]
normalTerm
  = [ word
    , quotation
    , text
    , integer
    , inexact
    ]

definition :: TokenParser Term
definition = do
  name  <- word <* token Token.Definition
  body  <- choice normalTerm
  body' <- case body of
    (Term.Word _)         -> return $ Term.Quotation [body]
    (Term.Integer _)      -> return $ Term.Quotation [body]
    (Term.Inexact _)      -> return $ Term.Quotation [body]
    (Term.Quotation _)    -> return body
    (Term.Definition _ _) -> unexpected "definition"
  return $ Term.Definition name body'

quotation :: TokenParser Term
quotation = Term.Quotation <$> choice [plain, layout]
  where
    plain  = left *> many term <* right <?> "plain quotation"
    left   = token Token.QuotationOpen
    right  = token Token.QuotationClose <?> "end of quotation"
    layout = do
      (Token.Located startLocation _) <- locatedToken Token.Layout
      tokens <- many . locatedSatisfy $ \ (Token.Located location _) ->
        sourceColumn location > sourceColumn startLocation
      case parse' program' tokens "layout quotation" of
        Right result -> return result
        Left err     -> fail $ show err

word :: TokenParser Term
word = toTerm <$> satisfy isWord
  where
    isWord (Token.Word _) = True
    isWord _              = False
    toTerm (Token.Word w) = Term.Word w
    toTerm _              = $(impossible)

inexact :: TokenParser Term
inexact = toTerm <$> satisfy isInexact
  where
    isInexact (Token.Inexact _) = True
    isInexact _                 = False
    toTerm (Token.Inexact f)    = Term.Inexact f
    toTerm _                    = $(impossible)

integer :: TokenParser Term
integer = toTerm <$> satisfy isInteger
  where
    isInteger (Token.Integer _) = True
    isInteger _                 = False
    toTerm (Token.Integer i)    = Term.Integer i
    toTerm _                    = $(impossible)

text :: TokenParser Term
text = toTerm <$> satisfy isText
  where
    isText (Token.Text _) = True
    isText _              = False
    toTerm (Token.Text t)
      = Term.Quotation . map (Term.Integer . fromIntegral . ord)
      $ Text.unpack t
    toTerm _ = $(impossible)

advance
  :: SourcePos
  -> t
  -> [Token.Located]
  -> SourcePos
advance _ _ (Token.Located sourcePos _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy
  :: (Token.Token -> Bool)
  -> TokenParser Token.Token
satisfy predicate = tokenPrim show advance
  $ \ (Token.Located _ tok) -> boolToMaybe (predicate tok) tok

locatedSatisfy
  :: (Token.Located -> Bool)
  -> TokenParser Token.Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ loc -> boolToMaybe (predicate loc) loc

token :: Token.Token -> TokenParser Token.Token
token tok = satisfy (== tok)

locatedToken :: Token.Token -> TokenParser Token.Located
locatedToken tok = locatedSatisfy (\ (Token.Located _ loc) -> loc == tok)

anyLocatedToken :: TokenParser Token.Located
anyLocatedToken = locatedSatisfy (const True)
