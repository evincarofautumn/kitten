module Parse 
  ( Parse.parse
  ) where

import Program
import Term

import qualified Token
import Tokenize
import Utils

import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad
import Control.Monad.Identity
import Data.Char
import qualified Text as Text
import Text.Parsec.Error as Parsec
import Text.Parsec.Pos as Parsec
import Text.Parsec.Prim as Parsec hiding (token, tokens)
import Text.Parsec.Combinator as Parsec

advance :: SourcePos -> t -> [Token.Located] -> SourcePos
advance _ _ (Token.Located sourcePos _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy
  :: (Monad m)
  => (Token.Token -> Bool)
  -> ParsecT [Token.Located] u m Token.Token
satisfy predicate = tokenPrim show advance
  $ \ (Token.Located _ tok) -> boolToMaybe (predicate tok) tok

type TokenParser a = ParsecT [Token.Located] () Identity a

parse :: String -> String -> Either ParseError Program
parse source name
  = case tokenize source name of
      Right result -> Parsec.runParser program () name result
      Left err -> Left err

program :: TokenParser Program
program = Program <$> many term <* eof

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

token :: Token.Token -> TokenParser ()
token t = void $ satisfy (== t)

definition :: TokenParser Term
definition = do
  name <- word <* token Token.Definition
  body <- choice normalTerm
  body' <- case body of
    (Term.Word _) -> return $ Term.Quotation [body]
    (Term.Integer _) -> return $ Term.Quotation [body]
    (Term.Inexact _) -> return $ Term.Quotation [body]
    (Term.Quotation _) -> return body
    (Term.Definition _ _) -> unexpected "definition"
  return $ Term.Definition name body'

quotation :: TokenParser Term
quotation = Term.Quotation <$> (left *> many term <* right) <?> "quotation"
  where
    left = token Token.QuotationOpen
    right = token Token.QuotationClose <?> "end of quotation"

word :: TokenParser Term
word = toTerm <$> satisfy isWord
  where
    isWord (Token.Word _) = True
    isWord _ = False
    toTerm (Token.Word w) = Term.Word w
    toTerm _ = undefined

inexact :: TokenParser Term
inexact = toTerm <$> satisfy isInexact
  where
    isInexact (Token.Inexact _) = True
    isInexact _ = False
    toTerm (Token.Inexact f) = Term.Inexact f
    toTerm _ = undefined

integer :: TokenParser Term
integer = toTerm <$> satisfy isInteger
  where
    isInteger (Token.Integer _) = True
    isInteger _ = False
    toTerm (Token.Integer i) = Term.Integer i
    toTerm _ = undefined

text :: TokenParser Term
text = toTerm <$> satisfy isText
  where
    isText (Token.Text _) = True
    isText _ = False
    toTerm (Token.Text t)
      = Term.Quotation . map (Term.Integer . fromIntegral . ord) $ Text.unpack t
    toTerm _ = undefined
