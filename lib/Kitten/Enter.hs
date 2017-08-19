{-|
Module      : Kitten.Enter
Description : Inserting entries into the dictionary
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Enter
  ( fragment
  , fragmentFromSource
  , resolveAndDesugar
  ) where

import Control.Monad ((>=>))
import Data.Foldable (foldlM)
import Data.Text (Text)
import Kitten.Bracket (bracket)
import Kitten.Closure (convertClosures)
import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Dictionary (Dictionary)
import Kitten.Flatten (flatten)
import Kitten.Fragment (Fragment)
import Kitten.Infer (mangleInstance, typecheck)
import Kitten.Informer (checkpoint, report)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Metadata (Metadata)
import Kitten.Monad (K)
import Kitten.Name
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..))
import Kitten.Tokenize (tokenize)
import Kitten.TypeDefinition (TypeDefinition)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Declaration as Declaration
import qualified Kitten.Definition as Definition
import qualified Kitten.Desugar.Infix as Infix
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Metadata as Metadata
import qualified Kitten.Parse as Parse
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Quantify as Quantify
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.TypeDefinition as TypeDefinition
import qualified Text.PrettyPrint as Pretty

-- | Enters a program fragment into a dictionary.

fragment :: Fragment 'Parsed -> Dictionary -> K Dictionary
fragment f
  -- TODO: Link constructors to parent type.
  = foldlMx declareType (Fragment.types f)
  -- We enter declarations of all traits and intrinsics.
  >=> foldlMx enterDeclaration (Fragment.declarations f)
  -- Then declare all permissions.
  >=> foldlMx declareWord
    (filter ((== Category.Permission) . Definition.category)
      $ Fragment.definitions f)
  -- With everything type-level declared, we can resolve type signatures.
  >=> foldlMx resolveSignature
    (map Declaration.name (Fragment.declarations f))
  -- And declare regular words.
  >=> foldlMx declareWord
    (filter ((/= Category.Permission) . Definition.category)
      $ Fragment.definitions f)
  -- Then resolve their signatures.
  >=> foldlMx resolveSignature
    (map Definition.name (Fragment.definitions f)
      ++ map Declaration.name (Fragment.declarations f))
  -- Add their metadata (esp. for operator metadata).
  >=> foldlMx addMetadata (Fragment.metadata f)
  -- And finally enter their definitions.
  >=> foldlMx defineWord (Fragment.definitions f)
  where
  foldlMx :: (Foldable f, Monad m) => (b -> a -> m b) -> f a -> b -> m b
  foldlMx = flip . foldlM

enterDeclaration :: Dictionary -> Declaration -> K Dictionary
enterDeclaration dictionary declaration = do
  let
    name = Declaration.name declaration
    signature = Declaration.signature declaration
    origin = Declaration.origin declaration
  case Dictionary.lookup (Instantiated name []) dictionary of
    -- TODO: Check signatures.
    Just _existing -> return dictionary
    Nothing -> case Declaration.category declaration of
      Declaration.Intrinsic -> do
        let
          entry = Entry.Word
            Category.Word
            Merge.Deny
            origin
            -- FIXME: Does a declaration ever need a parent?
            Nothing
            (Just signature)
            Nothing
        return $ Dictionary.insert (Instantiated name []) entry dictionary
      Declaration.Trait -> do
        let entry = Entry.Trait origin signature
        return $ Dictionary.insert (Instantiated name []) entry dictionary

-- declare type, declare & define constructors
declareType :: Dictionary -> TypeDefinition -> K Dictionary
declareType dictionary type_ = let
  name = TypeDefinition.name type_
  in case Dictionary.lookup (Instantiated name []) dictionary of
    -- Not previously declared.
    Nothing -> do
      let
        entry = Entry.Type
          (TypeDefinition.origin type_)
          (TypeDefinition.parameters type_)
          (TypeDefinition.constructors type_)
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    -- Previously declared with the same parameters.
    Just (Entry.Type _origin parameters _ctors)
      | parameters == TypeDefinition.parameters type_
      -> return dictionary
    -- Already declared or defined differently.
    Just{} -> error $ Pretty.render $ Pretty.hsep
      [ "type"
      , Pretty.quote name
      , "already declared or defined differently"
      ]

declareWord
  :: Dictionary -> Definition 'Parsed -> K Dictionary
declareWord dictionary definition = let
  name = Definition.name definition
  signature = Definition.signature definition
  in case Dictionary.lookup (Instantiated name []) dictionary of
    -- Not previously declared or defined.
    Nothing -> do
      let
        entry = Entry.Word
          (Definition.category definition)
          (Definition.merge definition)
          (Definition.origin definition)
          (Definition.parent definition)
          (Just signature)
          Nothing
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    -- Already declared with the same signature.
    Just (Entry.Word _ _ originalOrigin _ mSignature _)
      | Definition.inferSignature definition || mSignature == Just signature
      -> return dictionary
      | otherwise
      -> do
         report $ Report.WordRedeclaration (Signature.origin signature)
           name signature originalOrigin mSignature
         return dictionary
    -- Already declared or defined as a trait.
    Just (Entry.Trait _origin traitSignature)
      -- TODO: Better error reporting when a non-instance matches a trait.
      | Definition.category definition == Category.Instance
      -> do
      let qualifier = qualifierName name
      resolvedSignature <- Resolve.run $ Resolve.signature
        dictionary qualifier $ Definition.signature definition
      mangledName <- mangleInstance dictionary
        (Definition.name definition)
        resolvedSignature traitSignature
      let
        entry = Entry.Word
          (Definition.category definition)
          (Definition.merge definition)
          (Definition.origin definition)
          (Definition.parent definition)
          (Just resolvedSignature)
          Nothing
      return $ Dictionary.insert mangledName entry dictionary
    -- Already declared or defined with a different signature.
    Just{} -> error $ Pretty.render $ Pretty.hsep
      [ "word"
      , Pretty.quote name
      , "already declared or defined without signature or as a non-word"
      ]

addMetadata :: Dictionary -> Metadata -> K Dictionary
addMetadata dictionary0 metadata
  = foldlM addField dictionary0 $ HashMap.toList $ Metadata.fields metadata
  where
  QualifiedName qualified = Metadata.name metadata
  origin = Metadata.origin metadata
  qualifier = qualifierFromName qualified

  addField :: Dictionary -> (Unqualified, Sweet 'Parsed) -> K Dictionary
  addField dictionary (unqualified, term) = do
    let name = Qualified qualifier unqualified
    case Dictionary.lookup (Instantiated name []) dictionary of
      Just{} -> return dictionary  -- TODO: Report duplicates or merge?
      Nothing -> return
        $ Dictionary.insert (Instantiated name [])
        (Entry.Metadata origin term) dictionary

resolveSignature :: Dictionary -> Qualified -> K Dictionary
resolveSignature dictionary name = do
  let qualifier = qualifierName name
  case Dictionary.lookup (Instantiated name []) dictionary of
    Just (Entry.Word category merge origin parent (Just signature) body) -> do
      signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
      let
        entry = Entry.Word category merge origin parent (Just signature') body
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    Just (Entry.Trait origin signature) -> do
      signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
      let entry = Entry.Trait origin signature'
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    _ -> return dictionary

-- typecheck and define user-defined words
-- desugaring of operators has to take place here
defineWord
  :: Dictionary
  -> Definition 'Parsed
  -> K Dictionary
defineWord dictionary definition = do
  let name = Definition.name definition
  resolved <- resolveAndDesugar dictionary definition
  checkpoint
  let resolvedSignature = Definition.signature resolved
  -- Note that we use the resolved signature here.
  (typecheckedBody, type_) <- typecheck dictionary
    (if Definition.inferSignature definition
      then Nothing
      else Just resolvedSignature)
    $ Definition.body resolved
  checkpoint
  case Dictionary.lookup (Instantiated name []) dictionary of
    -- Already declared or defined as a trait.
    Just (Entry.Trait _origin traitSignature)
      | Definition.category definition == Category.Instance
      -> do
      mangledName <- mangleInstance dictionary name
        resolvedSignature traitSignature
      -- Should this use the mangled name?
      (flattenedBody, dictionary') <- flatten
        dictionary
        (qualifierFromName name)
        $ Quantify.term type_ typecheckedBody
      let
        entry = Entry.Word
          (Definition.category definition)
          (Definition.merge definition)
          (Definition.origin definition)
          (Definition.parent definition)
          (Just resolvedSignature)
          (Just flattenedBody)
      return $ Dictionary.insert mangledName entry dictionary'
    -- Previously declared with same signature, but not defined.
    Just (Entry.Word category merge origin' parent signature' Nothing)
      | maybe True (resolvedSignature ==) signature' -> do
      (flattenedBody, dictionary') <- flatten
        dictionary (qualifierFromName name)
        $ Quantify.term type_ typecheckedBody
      let
        entry = Entry.Word
          category merge origin' parent
          (Just $ if Definition.inferSignature definition
            then Signature.Type type_
            else resolvedSignature)
          $ Just flattenedBody
      return $ Dictionary.insert (Instantiated name []) entry dictionary'

    -- Already defined as concatenable.
    Just (Entry.Word category merge@Merge.Compose
      origin' parent mSignature body)
      | Definition.inferSignature definition
        || Just resolvedSignature == mSignature -> do
      composedBody <- case body of
        Just existing -> do
          let strippedBody = Term.discardTypes existing
          return $ SCompose () strippedBody $ Definition.body resolved
        Nothing -> return $ Definition.body resolved
      (composed, composedType) <- typecheck dictionary
        (if Definition.inferSignature definition
          then Nothing
          else Just resolvedSignature)
        composedBody
      (flattenedBody, dictionary') <- flatten
        dictionary (qualifierFromName name)
        $ Quantify.term composedType composed
      let
        entry = Entry.Word category merge origin' parent
          (if Definition.inferSignature definition
            then Nothing -- Just (Signature.Type composedType)
            else mSignature)
          $ Just flattenedBody
      return $ Dictionary.insert (Instantiated name []) entry dictionary'

    -- Already defined, not concatenable.
    Just (Entry.Word _ Merge.Deny originalOrigin _ (Just _sig) _) -> do
      report $ Report.WordRedefinition (Definition.origin definition)
        name originalOrigin
      return dictionary
    -- Not previously declared as word.
    _ -> error $ Pretty.render $ Pretty.hsep
      [ "defining word"
      , Pretty.quote name
      , "not previously declared"
      ]

-- | Parses a source file into a program fragment.

fragmentFromSource
  :: [GeneralName]
  -- ^ List of permissions granted to @main@.
  -> Maybe Qualified
  -- ^ Override name of @main@.
  -> Int
  -- ^ Initial source line (e.g. for REPL offset).
  -> FilePath
  -- ^ Source file path for error reporting.
  -> Text
  -- ^ Source itself.
  -> K (Fragment 'Parsed)
  -- ^ Parsed program fragment.
fragmentFromSource mainPermissions mainName line path source = do

-- Sources are lexed into a stream of tokens.

  tokenized <- tokenize line path source
  checkpoint

-- The layout rule is applied to desugar indentation-based syntax, so that the
-- parser can find the ends of blocks without checking the indentation of
-- tokens.

  bracketed <- bracket path tokenized

-- We then parse the token stream as a series of top-level program elements.
-- Datatype definitions are desugared into regular definitions, so that name
-- resolution can find their names.

  parsed <- Parse.fragment line path mainPermissions mainName bracketed
  checkpoint

  return parsed

resolveAndDesugar :: Dictionary -> Definition 'Parsed -> K (Definition 'Scoped)
resolveAndDesugar dictionary definition = do

-- Name resolution rewrites unqualified names into fully qualified names, so
-- that it's evident from a name which program element it refers to.

  -- needs dictionary for declared names
  resolved <- Resolve.run $ Resolve.definition dictionary definition
  checkpoint

-- After names have been resolved, the precedences of operators are known, so
-- infix operators can be desugared into postfix syntax.

  -- needs dictionary for operator metadata
  postfix <- Infix.desugar dictionary resolved
  checkpoint

-- In addition, now that we know which names refer to local variables,
-- quotations can be rewritten into closures that explicitly capture the
-- variables they use from the enclosing scope.

  return postfix { Definition.body = convertClosures $ Definition.body postfix }
