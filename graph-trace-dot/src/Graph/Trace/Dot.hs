{-# LANGUAGE OverloadedStrings #-}
module Graph.Trace.Dot
  ( parseLogEntries
  , parseLogEntry
  , buildGraph
  , graphToDot
  , Key(..)
  , LogEntry(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Attoparsec.ByteString.Lazy as AttoL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Ord (Down(..))
import           Data.Semigroup (Min(..))

parseLogEntries :: BSL.ByteString -> Either String [LogEntry]
parseLogEntries = AttoL.parseOnly (Atto.many' parseLogEntry <* Atto.endOfInput)

data Key = Key { keyId :: !Word, keyName :: !BSL.ByteString }
  deriving (Eq, Ord, Show)

data LogEntry
  = Entry Key (Maybe Key)
  | Trace Key BSL.ByteString
  deriving Show

-- | Use this to escape special characters that appear in the HTML portion of
-- the dot code. Other strings such as node names should not be escaped.
htmlEscape :: BSL.ByteString -> BSL.ByteString
htmlEscape bs = foldl' doReplacement bs replacements
  where
    doReplacement acc (c, re) =
      case BSL8.break (== c) acc of
        (before, after)
          | BSL.null after -> acc
          | otherwise -> before <> re <> BSL8.tail after

    replacements =
      [ ('<', "&lt;")
      , ('>', "&gt;")
      , ('\\', "\\\\")
      , ('&', "&amp;")
      ]

parseKey :: Atto.Parser Key
parseKey = do
  kName <- Atto.takeTill (== '§') <* Atto.char '§'
  kId <- Atto.decimal <* Atto.char '§'
  pure $ Key { keyId = kId, keyName = BSL.fromStrict kName }

parseLogEntry :: Atto.Parser LogEntry
parseLogEntry = (parseEntryEvent <|> parseTraceEvent) <* Atto.many' Atto.space

parseEntryEvent :: Atto.Parser LogEntry
parseEntryEvent = do
  _ <- Atto.string "entry§"
  curKey <- parseKey
  mPrevKey <- Just <$> parseKey
          <|> Nothing <$ Atto.string "§§"
  _ <- Atto.many' Atto.space
  pure $ Entry curKey mPrevKey

parseTraceEvent :: Atto.Parser LogEntry
parseTraceEvent = do
  _ <- Atto.string "trace§"
  key <- parseKey
  message <- Atto.takeTill (== '§') <* Atto.char '§'
  _ <- Atto.many' Atto.space
  pure $ Trace key (htmlEscape $ BSL.fromStrict message)

data NodeEntry
  = Message BSL.ByteString -- ^ The trace message
  | Edge Key -- ^ Id of the invocation to link to
  deriving Show

-- Remembers the order in which the elements were inserted
type Graph = M.Map Key (Min Int, [NodeEntry])

buildGraph :: [LogEntry] -> Graph
buildGraph = foldl' build mempty where
  build graph entry =
    case entry of
      Trace tag msg ->
        M.insertWith (<>) tag (graphSize, [Message msg]) graph
      Entry curTag (Just prevTag) ->
          M.insertWith (<>) curTag (graphSize + 1, [])
        $ M.insertWith (<>) prevTag (graphSize, [Edge curTag]) graph
      Entry curTag Nothing ->
        M.insertWith (<>) curTag (graphSize, []) graph
    where
      graphSize = Min $ M.size graph

graphToDot :: Graph -> BSB.Builder
graphToDot graph = header <> graphContent <> "}"
  where
    orderedEntries = map (fmap snd)
                   . List.sortOn (Down . fst . snd)
                   $ M.toList graph
    graphContent =
      -- knot-tying is used to get the color of a node from the edge pointing to that node.
      -- TODO consider doing separate traversals for edges and nodes so that the
      -- result can be built strictly.
      let (output, _, colorMap) =
            foldl'
              (doNode colorMap)
              (mempty, cycle edgeColors, mempty)
              orderedEntries
       in output

    header :: BSB.Builder
    header = "digraph {\nnode [tooltip=\" \" shape=plaintext colorscheme=set28]\n"

    doNode finalColorMap (acc, colors, colorMapAcc) (key, entries) =
      let (cells, edges, colors', colorMapAcc')
            = foldl' doEntry ([], [], colors, colorMapAcc) (zip entries [1..])
          acc' =
            if null entries && isJust mEdgeColor
               then acc
               else tableStart
                 <> labelCell
                 <> mconcat cells
                 <> tableEnd
                 <> mconcat edges
                 <> acc
       in (acc', colors', colorMapAcc')
      where
        keyStr (Key i k) = BSB.lazyByteString k <> BSB.wordDec i
        quoted bs = "\"" <> bs <> "\""
        mEdgeColor = M.lookup key finalColorMap
        nodeColor = case mEdgeColor of
                      Nothing -> ""
                      Just c -> "BGCOLOR=\"" <> c <> "\" "
        labelCell = "<TR><TD " <> nodeColor <> "><B>"
                 <> BSB.lazyByteString (htmlEscape $ keyName key) <> "</B></TD></TR>\n"
        tableStart = quoted (keyStr key) <> " [label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
        tableEnd :: BSB.Builder
        tableEnd = "</TABLE>>];"

        doEntry (cs, es, colors'@(color:nextColors), colorMap) ev = case ev of
          (Message str, idx) ->
            let el = "<TR><TD ALIGN=\"LEFT\" PORT=\""
                  <> BSB.wordDec idx <> "\">"
                  <> BSB.lazyByteString str <> "</TD></TR>"
             in (el : cs, es, colors', colorMap)
          (Edge edgeKey, idx) ->
            let href =
                  case mEdge of
                    Nothing -> mempty
                    Just _ -> " HREF=\"#" <> keyStr edgeKey { keyName = htmlEscape $ keyName edgeKey } <> "\""
                el = "<TR><TD ALIGN=\"LEFT\" CELLPADDING=\"1\" BGCOLOR=\""
                  <> color <> "\" PORT=\"" <> BSB.wordDec idx <> "\""
                  <> href
                  <> "><FONT POINT-SIZE=\"8\">"
                  <> BSB.lazyByteString (htmlEscape $ keyName edgeKey)
                  <> "</FONT></TD></TR>"
                mEdge = do
                  (_, targetContent) <- M.lookup edgeKey graph
                  guard . not $ null targetContent
                  Just $
                    quoted (keyStr key) <> ":" <> BSB.wordDec idx
                    <> " -> " <> quoted (keyStr edgeKey)
                    <> " [colorscheme=set28 color=" <> color <> "];"

             in ( el : cs
                , maybe id (:) mEdge es
                , nextColors
                , M.insert edgeKey color colorMap
                )
        doEntry ac _ = ac

edgeColors :: [BSB.Builder]
edgeColors = BSB.intDec <$> [1..8 :: Int]
