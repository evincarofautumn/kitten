module Kitten.Entry.Word
  ( Entry(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Entry.Category (Category)
import Kitten.Entry.Parent (Parent)
import Kitten.Name (Unqualified)
import Kitten.Operator (Associativity, Precedence)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import Kitten.Type (Type)

data Entry = Entry

-- If present, the associativity (leftward or rightward) of this operator; if
-- not, defaults to non-associative.

  { associativity :: !(Maybe Associativity)

-- Whether this is a word/instance, trait, or permission.

  , category :: !Category

-- If present, the definition of the word; if not, this is a declaration.

  , body :: !(Maybe (Term Type))

-- Whether this word is visible outside its vocabulary.

  , export :: !Bool

-- User-defined metadata.

  , metadata :: !(HashMap Unqualified (Term ()))

-- Source location.

  , origin :: !Origin

-- If present, the precedence of this operator; if not, defaults to 6.

  , precedence :: !(Maybe Precedence)

-- The type signature of this definition or declaration.

  , signature :: !Signature

-- If present, the trait declaration of which this definition is an instance, or
-- the type of which this definition is a constructor; if not, this is a normal
-- definition.

  , parent :: !(Maybe Parent)

  } deriving (Show)
