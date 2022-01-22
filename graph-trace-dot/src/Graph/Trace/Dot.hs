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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
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
  deriving Show

-- Remembers the order in which the elements were inserted. Is monoidal
type Node key =
  ( Min Int -- order
  , ( [NodeEntry key] -- contents
    , Alt Maybe SrcCodeLoc -- definition site
    , Alt Maybe (Maybe key) -- back link. Is Just Nothing if there are multiple incoming edges (nexus)
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
parseLogEntries = AttoL.parseOnly (AttoL.many' parseLogEntry <* AttoL.endOfInput)

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
buildTree = foldl' build mempty where
  build graph entry =
    case entry of
      Trace tag msg callSite ->
        M.insertWith (<>)
          tag
          (graphSize, ([Message msg callSite], mempty, mempty))
          graph

      Entry curTag (Just prevTag) defSite callSite ->
        let graph' =
              M.insertWith (<>)
                prevTag
                (graphSize, ([Edge curTag callSite], mempty, mempty))
                graph
         in M.insertWith (<>)
              curTag
              (Min $ M.size graph'
                , ([], Alt defSite, Alt . Just $ Just prevTag))
              graph'

      Entry curTag Nothing defSite _ ->
        M.insertWith (<>)
          curTag
          (graphSize, ([], Alt defSite, mempty))
          graph
    where
      graphSize = Min $ M.size graph

-- | Constructs a nexus by merging tree nodes that have identical content based
-- on their hash.
buildNexus :: Tree -> Nexus
buildNexus tree =
  let hashes = calcHashes tree
      toNexusKey key =
        case M.lookup key hashes of
          Nothing -> error "missing hash"
          Just hash ->
            NexusKey { nexKeyName = keyName key, nexKeyHash = hash }

      mapNode ((order, (entries, loc, mKey)), multipleParents) =
        (order, ( mapEntry <$> entries
                , loc
                , if multipleParents
                     then Alt (Just Nothing)
                     else fmap toNexusKey <$> mKey
                )
        )

      mapEntry = \case
        Message msg loc -> Message msg loc
        Edge key loc ->
          let nexKey = toNexusKey key
           in Edge nexKey
                   loc

      -- Used to determine if a node has inbound edges from two different parents
      multipleInEdges
          a@((_, (_, _, Alt ia)), multInA)
          ((_, (_, _, Alt ib)), multInB) =
        case (==) <$> (toNexusKey <$> join ia) <*> (toNexusKey <$> join ib) of
          (Just False) -> (fst a, True)
          _ -> (fst a, multInA || multInB)

   in M.map mapNode $
        M.mapKeysWith
          multipleInEdges
          toNexusKey
          ((,False) <$> tree)

-- | Produce a mapping of tree keys to the hash for that node.
calcHashes :: Tree -> M.Map Key BS.ByteString
calcHashes tree =
  -- this relies on knot tying and must therefore use the lazy Map api
  let hashes = ML.foldlWithKey buildHash mempty tree

      buildHash acc key (_, (entries, defSite, _)) =
        let entryHashes = foldMap hashEntry entries
            hash = Base16.encode . Sha.hash
                 $ keyName key
                <> BSL.toStrict (BSB.toLazyByteString entryHashes)
                <> BS8.pack (show defSite)
         in ML.insert key hash acc

      hashEntry = \case
        Message msg loc ->
          BSB.byteString . Base16.encode . Sha.hash
            $ msg <> BS8.pack (show loc)
        Edge key _ -> BSB.byteString $ ML.findWithDefault mempty key hashes
   in hashes

--------------------------------------------------------------------------------
-- Dot
--------------------------------------------------------------------------------

graphToDot :: IsKey key => Graph key -> BSB.Builder
graphToDot graph = header <> graphContent <> "}"
  where
    orderedEntries = map (second snd)
                   . List.sortOn (Down . fst . snd)
                   $ M.toList graph
    graphContent =
      let (output, _, colorMap) =
            foldl'
              (doNode colorMap)
              (mempty, cycle edgeColors, mempty)
              orderedEntries
       in output

    header :: BSB.Builder
    header = "digraph {\nnode [tooltip=\" \" shape=plaintext colorscheme=set28]\n"

    doNode finalColorMap (acc, colors, colorMapAcc)
                         (key, (entries, Alt mSrcLoc, Alt mBacklink)) =
      let (cells, edges, colors', colorMapAcc')
            = foldl' doEntry ([], [], colors, colorMapAcc) (zip entries [1..])
          acc' =
            -- don't render nodes that have inbound edge(s) but no content
            if null entries && isJust mBacklink
               then acc
               else tableStart
                 <> tableEl cells
                 <> tableEnd
                 <> mconcat edges
                 <> acc
       in (acc', colors', colorMapAcc')
      where
        quoted bs = "\"" <> bs <> "\""
        -- Building a node
        nodeColor = ML.lookup key finalColorMap
        nodeToolTip = foldMap (("defined at " <>) . pprSrcCodeLoc) mSrcLoc
        backHref = (foldMap . foldMap) (\k -> "#" <> keyStr k) mBacklink
        labelCell =
          el "TR" []
            [ el "TD" [ "HREF" .= backHref
                      , "TOOLTIP" .= nodeToolTip
                      , "BGCOLOR" .=? nodeColor
                      ]
                [ (foldMap . foldMap) (const $ el "FONT" ["POINT-SIZE" .= "7"] ["&larr;"])
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
        doEntry (cs, es, colors'@(color:nextColors), colorMap) = \case
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
             in (msgEl : cs, es, colors', colorMap)
          (Edge edgeKey mCallSite, idx) ->
            let mTargetNode = M.lookup edgeKey graph
                -- If the target node isn't empty then check if a color is
                -- already assigned (for a nexus) otherwise use the next color.
                edgeColor =
                  case mTargetNode of
                    Just (_, (content, _, _))
                      | not (null content) ->
                          M.findWithDefault color edgeKey colorMap
                    _ -> color
                -- TODO ^ don't need this lookup if not a nexus
                href = foldMap (const $ "#" <> keyStrEsc edgeKey) mEdge
                elToolTip =
                  foldMap (("called at " <>) . pprSrcCodeLoc) mCallSite
                edgeEl =
                  el "TR" []
                    [ el "TD" [ "TOOLTIP" .= elToolTip
                              , "ALIGN" .= "LEFT"
                              , "CELLPADDING" .= "1"
                              , "BGCOLOR" .= edgeColor
                              , "PORT" .= BSB.wordDec idx
                              , "HREF" .= href
                              ]
                        [ el "FONT" [ "POINT-SIZE" .= "8" ]
                            [ BSB.byteString . htmlEscape $ getKeyName edgeKey ]
                        ]
                    ]

                mEdge = do
                  (_, (targetContent, _, _)) <- mTargetNode
                  guard . not $ null targetContent
                  Just $
                    quoted (keyStr key) <> ":" <> BSB.wordDec idx
                    <> " -> " <> quoted (keyStr edgeKey)
                    <> " [tooltip=\" \" colorscheme=set28 color=" <> edgeColor <> "];"

             in ( edgeEl : cs
                , maybe id (:) mEdge es
                , nextColors
                , M.insert edgeKey edgeColor colorMap
                )
        doEntry _ = mempty

type Element = BSB.Builder
type Attr = (BSB.Builder, Maybe BSB.Builder)
type Color = BSB.Builder

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
