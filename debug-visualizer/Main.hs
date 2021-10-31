import           Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           System.IO
import           Text.Read (readMaybe)

import qualified Debug.Internal.Types as DT

main :: IO ()
main = do
  logContents <- mapMaybe parseLogEvent . lines <$> readFile "debug_log.txt"
  let dotFileContent = graphToDot $ buildGraph logContents
  writeFile "debug.dot" dotFileContent

-- TODO use ByteString instead
parseLogEvent :: String -> Maybe DT.Event
parseLogEvent ln = case splitAt 6 ln of
  ("entry|", rest) -> do
    [curKey, curId, prevKey, prevId] <- pure $ breakLogLine 4 rest
    curId' <- readMaybe curId
    let mPrevId' = do
          pId <- readMaybe prevId
          pure $ DT.DT pId (Left prevKey)
    pure $ DT.EntryEvent (DT.DT curId' (Left curKey)) mPrevId'
  ("trace|", rest) -> do
    [curKey, curId, message] <- pure $ breakLogLine 3 rest
    curId' <- readMaybe curId
    pure $ DT.TraceEvent (DT.DT curId' (Left curKey)) message
  _ -> Nothing

breakLogLine :: Int -> String -> [String]
breakLogLine 0 inp = [inp]
breakLogLine n inp =
  let (chunk, rest) = break (== '|') inp
   in case rest of
        '|' : nxt -> chunk : breakLogLine (pred $! n) nxt
        _ -> [chunk]

data NodeEntry
  = Message String -- ^ The trace message
  | Edge (Word, String) -- ^ Id of the invocation to link to

buildGraph :: [DT.Event] -> M.Map (Word, String) [NodeEntry]
buildGraph = foldr build mempty where
  build (DT.TraceEvent tag msg)
    = M.insertWith (<>) (mkKey tag) [Message msg]
  build (DT.EntryEvent curTag (Just prevTag))
    = M.insertWith (<>) (mkKey curTag) []
    . M.insertWith (<>) (mkKey prevTag) [Edge $ mkKey curTag]
  build (DT.EntryEvent curTag Nothing)
    = M.insertWith (<>) (mkKey curTag) []
  mkKey (DT.DT invId eKey) = (invId, either id id eKey)

graphToDot :: M.Map (Word, String) [NodeEntry] -> String
graphToDot graph = header <> fst (M.foldlWithKey' doNode ("", cycle edgeColors) graph) <> "}"
  where
    header :: String
    header = "digraph {\nnode [shape=plaintext]\n"
    doNode (acc, colors) key entries =
      let (cells, edges, colors') = foldr doEntry ([], [], colors) (zip entries [1..])
          acc' = tableStart
              <> labelCell
              <> unlines cells
              <> tableEnd
              <> unlines edges
              <> acc
       in (acc', colors')
      where
        keyStr (i, k) = k <> show i
        labelCell = "<TR><TD BGCOLOR=\"grey94\"><B>" <> snd key <> "</B></TD></TR>\n"
        tableStart = keyStr key <> " [label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
        tableEnd = "</TABLE>>];"
        doEntry ev (cs, es, colors@(color:nextColors)) = case ev of
          (Message str, idx) ->
            let el = "<TR><TD PORT=\"" <> show idx <> "\">" <> str <> "</TD></TR>"
             in (el : cs, es, colors)
          (Edge edgeKey, idx) ->
            let el = "<TR><TD BGCOLOR=\"" <> color <> "\" PORT=\"" <> show idx
                  <> "\"></TD></TR>"
                edge = keyStr key <> ":" <> show idx <> " -> " <> keyStr edgeKey
                       <> " [color = " <> color <> "];"
             in (el : cs, edge : es, nextColors)

edgeColors :: [String]
edgeColors =
  [ "lightgreen"
  , "lightskyblue1"
  , "lightgoldenrod"
  , "lightcoral"
  , "lightsteelblue"
  , "navajowhite"
  , "plum"
  , "mediumturquoise"
  , "thistle"
  ]
