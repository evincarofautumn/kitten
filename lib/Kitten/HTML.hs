{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generates HTML from source code annotated with a Kitten
-- AST.
--
-- The algorithm operates in two passes:
--
-- First, the AST is walked, collecting a map from source
-- file to source position to a "node".  A node contains
-- information (e.g. type information) about source at a
-- position.
--
-- Second, the source code is walked.  Every time a node for
-- the current source position is encountered, an HTML start
-- tag is created representing that node.
module Kitten.HTML
  ( fromFragmentsM
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Text.Parsec.Pos as ParsecPos

import Kitten.Def
import Kitten.Fragment
import Kitten.Location
import Kitten.Tree
import Kitten.Type (Kind(..), Type, unScheme)
import Kitten.Util.Text (toText)

type LocMap = UnionMap FilePath (UnionMap Pos [Node])

data Node
  = Definition !Text
  | ScalarType !(Type Scalar)
  deriving (Show)  -- FIXME(strager): Temporary.

data NodeType
  = DefinitionType
  | ScalarTypeType

newtype UnionMap k a = UnionMap (Map k a)

instance (Monoid a, Ord k) => Monoid (UnionMap k a) where
  mempty = UnionMap mempty
  UnionMap a `mappend` UnionMap b = UnionMap
    $ Map.unionWith mappend a b

-- TODO(strager): Replace with a character offset.
data Pos = Pos !Int !Int
  deriving (Eq, Ord)

data ScanEnv = ScanEnv
  { envNodeMap :: !(Map Pos [Node])
  , envOpened :: [NodeType]
  , envPos :: !Pos
  }

fromFragmentsM
  :: forall m. (Monad m)
  => (FilePath -> m Text)
  -> [Fragment TypedTerm]
  -> m Text
fromFragmentsM lookUpSource fragments = do
  let UnionMap locMap = foldMap (execWriter . flattenFragment) fragments
  sources <- mapM showSource' $ Map.toList locMap
  return $ mconcat
    [ showHeader
    , showTableOfContents (Map.keys locMap)
    , Text.unlines sources
    ]
  where
  showSource'
    :: (FilePath, UnionMap Pos [Node])
    -> m Text
  showSource' (path, UnionMap nodeMap) = do
    source <- lookUpSource path
    return $ showSource path source nodeMap

-- * Flattening.

flattenFragment :: Fragment TypedTerm -> Writer LocMap ()
flattenFragment fragment = do
  V.mapM_ flattenDef (fragmentDefs fragment)
  V.mapM_ flattenTerm (fragmentTerms fragment)

flattenDef :: Def TypedTerm -> Writer LocMap ()
flattenDef def = do
  tellNode (defLocation def) $ Definition (defName def)
  flattenTerm $ unScheme (defTerm def)

flattenTerm :: TypedTerm -> Writer LocMap ()
flattenTerm theTerm = case theTerm of
  Builtin _builtin (loc, type_) -> tellNode loc $ ScalarType type_
  Call _ _ (loc, type_) -> tellNode loc $ ScalarType type_
  Compose _ terms (loc, type_) -> do
    tellNode loc $ ScalarType type_
    V.mapM_ flattenTerm terms
  Lambda _name term (loc, type_) -> do
    tellNode loc $ ScalarType type_
    flattenTerm term
  PairTerm a b (loc, type_) -> do
    tellNode loc $ ScalarType type_
    flattenTerm a >> flattenTerm b
  Push value (loc, type_) -> do
    tellNode loc $ ScalarType type_
    flattenValue value
  VectorTerm terms (loc, type_) -> do
    tellNode loc $ ScalarType type_
    V.mapM_ flattenTerm terms

flattenValue :: TypedValue -> Writer LocMap ()
flattenValue theValue = case theValue of
  Bool{} -> return ()
  Char{} -> return ()
  Closed{} -> return ()
  Closure _closed term _ -> flattenTerm term
  Float{} -> return ()
  Function{} -> return ()  -- ?
  Int{} -> return ()
  Local{} -> return ()
  String{} -> return ()

-- * Scanning and HTML generation.

-- | Shows the HTML header (DOCTYPE and scripts).
showHeader :: Text
showHeader
  = "<!DOCTYPE html>\n\
    \<link rel='stylesheet' href='http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css' />\
    \<script src='http://code.jquery.com/jquery-1.9.1.js'></script>\
    \<script src='http://code.jquery.com/ui/1.10.3/jquery-ui.js'></script>\
    \<style>\
    \  .ui-tooltip { max-width: inherit; font-size: 100%; }\
    \  code { font-size: 115%; }\
    \</style>\
    \<script>$(function () {\
    \  var noEffect = { duration: 0 };\
    \  $(document).tooltip({ show: noEffect, hide: noEffect, track: true });\
    \});</script>\n"

showTableOfContents :: [FilePath] -> Text
showTableOfContents paths = mconcat
  [ openTag "h1" []
  , htmlspecialchars "Source Files"
  , closeTag "h1"

  , openTag "ul" []
  , Text.unlines (map listItem paths)
  , closeTag "ul"
  ]
  where
  listItem :: FilePath -> Text 
  listItem path = mconcat
    [ openTag "li" []
    , openTag "a" [("href", "#file-" <> pathText)]
    , htmlspecialchars pathText
    , closeTag "a"
    , closeTag "li"
    ]
    where
    pathText :: Text
    pathText = Text.pack path

-- | Converts to HTML one source file annotated with node
-- information.
showSource :: FilePath -> Text -> Map Pos [Node] -> Text
showSource path source nodeMap = mconcat
  [ openTag "h2" [("id", "file-" <> pathText)]
  , htmlspecialchars pathText
  , closeTag "h2"

  , openTag "pre" [("data-filepath", pathText)]
  , openTag "code" []

  , runScanner $ mapM_ scanChar (Text.unpack source)

  , closeTag "code"
  , closeTag "pre"
  ]
  where
  pathText :: Text
  pathText = Text.pack path
  runScanner :: StateT ScanEnv (Writer Text) a -> Text
  runScanner m = execWriter $ evalStateT m ScanEnv
    { envNodeMap = nodeMap
    , envOpened = []
    , envPos = Pos 1 1
    }

openNode :: Node -> Writer Text ()
openNode node = tell . openTag (nodeTypeTag (nodeType node))
  $ case node of
    Definition _name -> []
    ScalarType type_ -> [("title", toText type_)]

closeNode :: NodeType -> Writer Text ()
closeNode type_ = tell $ closeTag (nodeTypeTag type_)

scanChar :: Char -> StateT ScanEnv (Writer Text) ()
scanChar c = do
  advanceChar c
  pos <- gets envPos
  nodeMap <- gets envNodeMap
  case Map.lookup pos nodeMap of
    Nothing -> return ()
    Just nodes -> do
      lift . mapM_ closeNode =<< gets envOpened
      lift $ mapM_ openNode nodes
      modify $ \env -> env
        { envOpened = map nodeType nodes }
  lift . tell $ htmlspecialchars (Text.singleton c)

-- * HTML utilities.

-- | Escapes a string for inserting in an HTML body or
-- attribute.
htmlspecialchars :: Text -> Text
htmlspecialchars = Text.concatMap escape
  where
  escape '"' = "&quot;"
  escape '&' = "&amp;"
  escape '<' = "&lt;"
  escape '>' = "&gt;"
  escape c = Text.singleton c

-- | Creates an open HTML tag.
openTag :: Text -> [(Text, Text)] -> Text
openTag name attributes
  = "<" <> name
  <> Text.concat (map ((" " <>) . mkAttribute) attributes)
  <> ">"
  where
  mkAttribute :: (Text, Text) -> Text
  mkAttribute (key, value) = key
    <> "='" <> htmlspecialchars value <> "'"

-- Creates a closing HTML tag.
closeTag :: Text -> Text
closeTag name = "</" <> name <> ">"

-- * Utilities.

advanceChar :: Char -> StateT ScanEnv (Writer Text) ()
advanceChar c = modify $ \env
  -> env { envPos = updatePos (envPos env) c }

locationPos :: Location -> Maybe (FilePath, Pos)
locationPos loc = case loc of
  Location start _indent -> Just
    ( ParsecPos.sourceName start
    , Pos (ParsecPos.sourceLine start) (ParsecPos.sourceColumn start)
    )
  _ -> Nothing

nodeType :: Node -> NodeType
nodeType (Definition _) = DefinitionType
nodeType (ScalarType _) = ScalarTypeType

nodeTypeTag :: NodeType -> Text
nodeTypeTag _ = "span"

tellNode :: Location -> Node -> Writer LocMap ()
tellNode location node = case locationPos location of
  Just (path, pos) -> tell
    . UnionMap . Map.singleton path
    . UnionMap $ Map.singleton pos [node]
  Nothing -> return ()

-- | Advances a 'Pos' by a character, a la
-- Text.Parsec.Pos.updatePosChar.
updatePos :: Pos -> Char -> Pos
updatePos (Pos line col) c = case c of
  '\n' -> Pos (line + 1) 1
  '\t' -> Pos line (col + 8 - ((col - 1) `mod` 8))
  _ -> Pos line (col + 1)
