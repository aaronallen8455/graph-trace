{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph.Trace.Dot
  ( parseLogEntries
  , parseLogEntry
  , buildTree
  , buildNexus
  , graphToDot
  , Key(..)
  , LogEntry(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import qualified Crypto.Hash.SHA256 as Sha
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Attoparsec.ByteString.Lazy as AttoL
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Monoid (Alt(..))
import           Data.Ord (Down(..))
import           Data.Semigroup (Min(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Key = Key { keyId :: !Word
               , keyName :: !BS.ByteString
               }
  deriving (Eq, Ord, Show)

data LogEntry
  = Entry
      Key
      (Maybe Key) -- called by
      (Maybe SrcCodeLoc) -- definition site
      (Maybe SrcCodeLoc) -- call site
  | Trace
      Key
      BS.ByteString
      (Maybe SrcCodeLoc)
  deriving Show

data SrcCodeLoc =
  SrcCodeLoc
    { srcMod :: BS.ByteString
    , srcLine :: Int
    , srcCol :: Int
    } deriving (Eq, Ord, Show)

data NodeEntry key
  = Message BS.ByteString  -- ^ The trace message
            (Maybe SrcCodeLoc) -- ^ call site
  | Edge key -- ^ Id of the invocation to link to
         (Maybe SrcCodeLoc) -- ^ call site
         Color
  deriving Show

type Color = BSB.Builder

-- Remembers the order in which the elements were inserted. Is monoidal
type Node key =
  ( Min Int -- order
  , ( [NodeEntry key] -- contents
    , Alt Maybe SrcCodeLoc -- definition site
    , Alt Maybe Color -- node color
    , Alt Maybe key -- back link
    )
  )

type Graph key = M.Map key (Node key)

type Tree = Graph Key

data NexusKey =
  NexusKey { nexKeyName :: !BS.ByteString, nexKeyHash :: !BS.ByteString }
  deriving (Eq, Ord, Show)

type Nexus = Graph NexusKey

class Ord key => IsKey key where
  getKeyName :: key -> BS.ByteString
  keyStr :: key -> BSB.Builder
  keyStrEsc :: key -> BSB.Builder

instance IsKey NexusKey where
  getKeyName = nexKeyName
  keyStr (NexusKey name hash) = BSB.byteString name <> BSB.byteString hash
  keyStrEsc k = keyStr k { nexKeyName = htmlEscape $ nexKeyName k }

instance IsKey Key where
  getKeyName = keyName
  keyStr (Key i k) = BSB.byteString k <> BSB.wordDec i
  keyStrEsc k = keyStr k { keyName = htmlEscape $ keyName k }

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseLogEntries :: BSL.ByteString -> Either String [LogEntry]
parseLogEntries = AttoL.parseOnly (Atto.many' parseLogEntry <* Atto.endOfInput)

parseKey :: Atto.Parser Key
parseKey = do
  kName <- Atto.takeTill (== '§') <* Atto.char '§'
  kId <- Atto.decimal <* Atto.char '§'
  pure $ Key { keyId = kId, keyName = kName }

parseLogEntry :: Atto.Parser LogEntry
parseLogEntry = (parseEntryEvent <|> parseTraceEvent) <* Atto.many' Atto.space

parseEntryEvent :: Atto.Parser LogEntry
parseEntryEvent = do
  _ <- Atto.string "entry§"
  curKey <- parseKey
  mPrevKey <- Just <$> parseKey
          <|> Nothing <$ Atto.string "§§"
  defSite <- parseSrcCodeLoc
  callSite <- parseSrcCodeLoc
  _ <- Atto.many' Atto.space
  pure $ Entry curKey mPrevKey defSite callSite

parseTraceEvent :: Atto.Parser LogEntry
parseTraceEvent = do
  _ <- Atto.string "trace§"
  key <- parseKey
  message <- Atto.takeTill (== '§') <* Atto.char '§'
  callSite <- parseSrcCodeLoc
  _ <- Atto.many' Atto.space
  let removeNewlines = BS8.unwords . BS8.lines
  pure $ Trace key (htmlEscape . removeNewlines $ message) callSite

parseSrcCodeLoc :: Atto.Parser (Maybe SrcCodeLoc)
parseSrcCodeLoc = do
  let parseLoc = do
        srcMod <- Atto.takeTill (== '§') <* Atto.char '§'
        srcLine <- Atto.decimal <* Atto.char '§'
        srcCol <- Atto.decimal <* Atto.char '§'
        pure SrcCodeLoc{..}
  Just <$> parseLoc <|> Nothing <$ Atto.string "§§§"

-- | Use this to escape special characters that appear in the HTML portion of
-- the dot code. Other strings such as node names should not be escaped.
htmlEscape :: BS.ByteString -> BS.ByteString
htmlEscape bs = foldl' doReplacement bs replacements
  where
    doReplacement acc (c, re) =
      case BS8.break (== c) acc of
        (before, after)
          | BS.null after -> acc
          | otherwise -> before <> re <> BS8.tail after

    replacements =
      [ ('&', "&amp;")
      , ('<', "&lt;")
      , ('>', "&gt;")
      , ('\\', "\\\\") -- not really an HTML escape, but still needed
      ]

--------------------------------------------------------------------------------
-- Graph construction
--------------------------------------------------------------------------------

buildTree :: [LogEntry] -> Tree
buildTree = fst . foldl' build (mempty, cycle edgeColors) where
  build (graph, colors@(color:colorTail)) entry =
    case entry of
      Trace tag msg callSite -> (,colors) $
        M.insertWith (<>)
          tag
          (graphSize, ([Message msg callSite], mempty, mempty, mempty))
          graph
      Entry curTag (Just prevTag) defSite callSite -> (,colorTail) .
          M.insertWith (<>)
            curTag
            (graphSize + 1, ([], Alt defSite, Alt $ Just color, Alt $ Just prevTag))
        $ M.insertWith (<>)
            prevTag
            (graphSize, ([Edge curTag callSite color], mempty, mempty, mempty))
            graph
      Entry curTag Nothing defSite _ -> (,colorTail) $
        M.insertWith (<>)
          curTag
          (graphSize, ([], Alt defSite, Alt $ Just color, mempty))
          graph
    where
      graphSize = Min $ M.size graph
  build acc _ = acc

-- | Constructs a nexus by merging tree nodes that have identical content based
-- on their hash.
buildNexus :: Tree -> Nexus
buildNexus tree =
  let hashes = calcHashes tree
      colorMap = M.fromList
               . mapMaybe (bitraverse pure id)
               $ M.elems hashes
      toNexusKey key =
        case M.lookup key hashes of
          Nothing -> error "missing hash"
          Just (hash, _) ->
            NexusKey { nexKeyName = keyName key, nexKeyHash = hash }
      mapNode ((order, (entries, loc, color, mKey)), multipleParents) =
        (order, ( mapEntry <$> entries
                , loc
                , color
                , guard (not multipleParents) >> toNexusKey <$> mKey
                )
        )
      mapEntry = \case
        Message msg loc -> Message msg loc
        Edge key loc color ->
          let nexKey = toNexusKey key
              mColor = M.lookup (nexKeyHash nexKey) colorMap
           in Edge nexKey
                   loc
                   (fromMaybe color mColor)
      multipleInEdges
          a@((_, (_, _, _, ia)), _)
          ((_, (_, _, _, ib)), _) =
        case (==) <$> (toNexusKey <$> ia) <*> (toNexusKey <$> ib) of
          Alt (Just False) -> (fst a, True)
          _ -> a
   in mapNode <$>
        M.mapKeysWith
          multipleInEdges
          toNexusKey
          ((,False) <$> tree)

calcHashes :: Tree -> M.Map Key (BS.ByteString, Maybe Color)
calcHashes tree =
  let hashes = ML.foldrWithKey go mempty tree
      go key = ML.insert key . hashNode
      hashNode (_, (entries, defSite, Alt mColor, _)) =
        ( Base16.encode . Sha.hash $
            foldMap hashEntry entries <> BS8.pack (show defSite)
        , mColor
        )
      hashEntry entry = case entry of
        Message{} -> BS8.pack (show entry)
        Edge key _ _ -> foldMap fst $ M.lookup key hashes
   in hashes

--------------------------------------------------------------------------------
-- Dot
--------------------------------------------------------------------------------

graphToDot :: IsKey key => Graph key -> BSB.Builder
graphToDot graph = header <> graphContent <> "}"
  where
    orderedEntries = map (second snd)
                   . List.sortOn (Down . fst)
                   $ M.toList graph
    graphContent =
      foldl'
        doNode
        mempty
        orderedEntries

    header :: BSB.Builder
    header = "digraph {\nnode [tooltip=\" \" shape=plaintext colorscheme=set28]\n"

    doNode acc (key, (entries, Alt mSrcLoc, Alt mColor, Alt mBacklink)) =
      let (cells, edges)
            = foldl' doEntry ([], []) (zip entries [1..])
          acc' =
            -- don't render nodes that have in inbound edge but no content
            if null entries && isJust mBacklink
               then acc
               else tableStart
                 <> tableEl cells
                 <> tableEnd
                 <> mconcat edges
                 <> acc
       in acc'
      where
        quoted bs = "\"" <> bs <> "\""
        -- Building a node
        nodeToolTip = foldMap (("defined at " <>) . pprSrcCodeLoc) mSrcLoc
        backHref = foldMap (\k -> "#" <> keyStr k) mBacklink
        labelCell =
          el "TR" []
            [ el "TD" [ "HREF" .= backHref
                      , "TOOLTIP" .= nodeToolTip
                      , "BGCOLOR" .=? mColor
                      ]
                [ foldMap (const $ el "FONT" ["POINT-SIZE" .= "7"] ["&larr;"])
                    mBacklink
                , " "
                , el "B" [] [ BSB.byteString . htmlEscape $ getKeyName key ]
                ]
            ]
        tableEl cells =
          el "TABLE" [ "BORDER" .= "0"
                     , "CELLBORDER" .= "1"
                     , "CELLSPACING" .= "0"
                     , "CELLPADDING" .= "4"
                     ]
            [ labelCell
            , mconcat cells
            ]
        tableStart, tableEnd :: BSB.Builder
        tableStart = quoted (keyStr key) <> " [label=<\n"
        tableEnd = ">];"

        -- Building an entry in a node
        doEntry (cs, es) ev = case ev of
          (Message str mCallSite, idx) ->
            let msgToolTip =
                  foldMap (("printed at " <>) . pprSrcCodeLoc) mCallSite
                msgEl =
                  el "TR" []
                    [ el "TD" [ "HREF" .= ""
                              , "TOOLTIP" .= msgToolTip
                              , "ALIGN" .= "LEFT"
                              , "PORT" .= BSB.wordDec idx
                              ]
                        [ BSB.byteString str ]
                    ]
             in (msgEl : cs, es)
          (Edge edgeKey mCallSite color, idx) ->
            let href = foldMap (const $ "#" <> keyStrEsc edgeKey) mEdge
                elToolTip =
                  foldMap (("called at " <>) . pprSrcCodeLoc) mCallSite
                edgeEl =
                  el "TR" []
                    [ el "TD" [ "TOOLTIP" .= elToolTip
                              , "ALIGN" .= "LEFT"
                              , "CELLPADDING" .= "1"
                              , "BGCOLOR" .= color
                              , "PORT" .= BSB.wordDec idx
                              , "HREF" .= href
                              ]
                        [ el "FONT" [ "POINT-SIZE" .= "8" ]
                            [ BSB.byteString . htmlEscape $ getKeyName edgeKey ]
                        ]
                    ]

                mEdge = do
                  (_, (targetContent, _, _, _)) <- M.lookup edgeKey graph
                  guard . not $ null targetContent
                  Just $
                    quoted (keyStr key) <> ":" <> BSB.wordDec idx
                    <> " -> " <> quoted (keyStr edgeKey)
                    <> " [tooltip=\" \" colorscheme=set28 color=" <> color <> "];"

             in ( edgeEl : cs
                , maybe id (:) mEdge es
                )

type Element = BSB.Builder
type Attr = (BSB.Builder, Maybe BSB.Builder)

(.=) :: BSB.Builder -> BSB.Builder -> Attr
name .= val = name .=? Just val

(.=?) :: BSB.Builder -> Maybe BSB.Builder -> Attr
name .=? val = (name, val)

el :: BSB.Builder -> [Attr] -> [BSB.Builder] -> Element
el name attrs children =
  "<" <> name <> foldMap renderAttr attrs <> ">"
  <> mconcat children <> "</" <> name <> ">"
  where
    renderAttr (aName, Just aVal) = " " <> aName <> "=\"" <> aVal <> "\""
    renderAttr (_, Nothing) = mempty

edgeColors :: [Color]
edgeColors = BSB.intDec <$> [1..8 :: Int]

pprSrcCodeLoc :: SrcCodeLoc -> BSB.Builder
pprSrcCodeLoc loc
  = BSB.byteString (srcMod loc) <> ":"
 <> BSB.intDec (srcLine loc) <> ":"
 <> BSB.intDec (srcCol loc)
