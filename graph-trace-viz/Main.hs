{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Builder as BSB
import           Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, isJust)
import           Data.Ord (Down(..))
import           Data.Semigroup (Min(..))
import           System.IO

main :: IO ()
main = do
  logContents <- mapMaybe parseLogEvent . BSL8.lines
             <$> BSL.readFile "debug_log.txt"
  let dotFileContent = graphToDot $ buildGraph logContents
  withFile "debug.dot" WriteMode $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering Nothing)
    BSB.hPutBuilder h dotFileContent

data Key = Key { keyId :: !Word, keyName :: !BSL.ByteString }
  deriving (Eq, Ord, Show)

data LogEntry
  = Entry Key (Maybe Key)
  | Trace Key BSL.ByteString
  deriving Show

parseLogEvent :: BSL.ByteString -> Maybe LogEntry
parseLogEvent ln = case BSL8.splitAt 6 ln of
  ("entry|", rest) -> do
    [keyName, curId, prevKey, prevId] <- pure $ breakLogLine rest
    (curId', _) <- BSL8.readInt curId
    let mPrevId' = do
          (pId, _) <- BSL8.readInt prevId
          pure $ Key (fromIntegral pId) prevKey
    pure $ Entry (Key (fromIntegral curId') keyName) mPrevId'
  ("trace|", rest) -> do
    [keyName, curId, message] <- pure $ breakLogLine rest
    (curId', _) <- BSL8.readInt curId
    pure $ Trace (Key (fromIntegral curId') keyName) message
  _ -> Nothing

breakLogLine :: BSL.ByteString -> [BSL.ByteString]
breakLogLine = BSL8.split '|'

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
        mEdgeColor = M.lookup key finalColorMap
        nodeColor = case mEdgeColor of
                      Nothing -> ""
                      Just c -> "BGCOLOR=\"" <> c <> "\" "
        labelCell = "<TR><TD " <> nodeColor <> "><B>"
                 <> BSB.lazyByteString (keyName key) <> "</B></TD></TR>\n"
        tableStart = keyStr key <> " [label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
        tableEnd :: BSB.Builder
        tableEnd = "</TABLE>>];"

        doEntry (cs, es, colors@(color:nextColors), colorMap) ev = case ev of
          (Message str, idx) ->
            let el = "<TR><TD ALIGN=\"LEFT\" PORT=\""
                  <> BSB.wordDec idx <> "\">"
                  <> BSB.lazyByteString str <> "</TD></TR>"
             in (el : cs, es, colors, colorMap)
          (Edge edgeKey, idx) ->
            let el = "<TR><TD ALIGN=\"LEFT\" CELLPADDING=\"1\" BGCOLOR=\""
                  <> color <> "\" PORT=\"" <> BSB.wordDec idx
                  <> "\"><FONT POINT-SIZE=\"8\">"
                  <> BSB.lazyByteString (keyName edgeKey)
                  <> "</FONT></TD></TR>"
                mEdge = do
                  (_, targetContent) <- M.lookup edgeKey graph
                  guard . not $ null targetContent
                  Just $
                    keyStr key <> ":" <> BSB.wordDec idx <> " -> " <> keyStr edgeKey
                    <> " [colorscheme=set28 color=" <> color <> "];"

             in ( el : cs
                , maybe id (:) mEdge es
                , nextColors
                , M.insert edgeKey color colorMap
                )

edgeColors :: [BSB.Builder]
edgeColors = BSB.intDec <$> [1..8 :: Int]
