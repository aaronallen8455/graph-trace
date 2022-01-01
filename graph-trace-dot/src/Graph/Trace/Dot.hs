{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Monoid (Alt(..))
import           Data.Ord (Down(..))
import           Data.Semigroup (Min(..))

parseLogEntries :: BSL.ByteString -> Either String [LogEntry]
parseLogEntries = AttoL.parseOnly (Atto.many' parseLogEntry <* Atto.endOfInput)

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
  | Trace Key BS.ByteString (Maybe SrcCodeLoc)
  deriving Show

data SrcCodeLoc =
  SrcCodeLoc
    { srcMod :: BS.ByteString
    , srcLine :: Int
    , srcCol :: Int
    } deriving (Eq, Ord, Show)

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

data NodeEntry
  = Message BS.ByteString  -- ^ The trace message
            (Maybe SrcCodeLoc) -- ^ call site
  | Edge Key -- ^ Id of the invocation to link to
         (Maybe SrcCodeLoc) -- ^ call site
  deriving Show

-- Remembers the order in which the elements were inserted
type Graph =
  M.Map Key ( Min Int -- order
            , [NodeEntry] -- contents
            , Alt Maybe SrcCodeLoc -- definition site
            )

buildGraph :: [LogEntry] -> Graph
buildGraph = foldl' build mempty where
  build graph entry =
    case entry of
      Trace tag msg callSite ->
        M.insertWith (<>)
          tag
          (graphSize, [Message msg callSite], Alt Nothing)
          graph
      Entry curTag (Just prevTag) defSite callSite ->
          M.insertWith (<>)
            curTag
            (graphSize + 1, [], Alt defSite)
        $ M.insertWith (<>)
            prevTag
            (graphSize, [Edge curTag callSite], Alt Nothing)
            graph
      Entry curTag Nothing defSite _ ->
        M.insertWith (<>) curTag (graphSize, [], Alt defSite) graph
    where
      graphSize = Min $ M.size graph

graphToDot :: Graph -> BSB.Builder
graphToDot graph = header <> graphContent <> "}"
  where
    orderedEntries = map (\(key, (_, content, srcLoc)) -> (key, (content, getAlt srcLoc)))
                   . List.sortOn (Down . (\(x,_,_) -> x) . snd)
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

    doNode finalColorMap (acc, colors, colorMapAcc) (key, (entries, mSrcLoc)) =
      let (cells, edges, colors', colorMapAcc')
            = foldl' doEntry ([], [], colors, colorMapAcc) (zip entries [1..])
          acc' =
            -- don't render nodes that have in inbound edge but no content
            if null entries && isJust mEdgeData
               then acc
               else tableStart
                 <> tableEl cells
                 <> tableEnd
                 <> mconcat edges
                 <> acc
       in (acc', colors', colorMapAcc')
      where
        keyStr (Key i k) = BSB.byteString k <> BSB.wordDec i
        keyStrEsc k = keyStr k { keyName = htmlEscape $ keyName k }
        quoted bs = "\"" <> bs <> "\""
        -- Building a node
        mEdgeData = M.lookup key finalColorMap
        nodeColor = snd <$> mEdgeData
        nodeToolTip = foldMap (("defined at " <>) . pprSrcCodeLoc) mSrcLoc
        backHref = foldMap (\(k, _) -> "#" <> keyStr k) mEdgeData
        labelCell =
          el "TR" []
            [ el "TD" [ "HREF" .= backHref
                      , "TOOLTIP" .= nodeToolTip
                      , "BGCOLOR" .=? nodeColor
                      ]
                [ foldMap (const $ el "FONT" ["POINT-SIZE" .= "7"] ["&larr;"])
                    mEdgeData
                , " "
                , el "B" [] [ BSB.byteString . htmlEscape $ keyName key ]
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
        doEntry (cs, es, colors'@(color:nextColors), colorMap) ev = case ev of
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
                            [ BSB.byteString . htmlEscape $ keyName edgeKey ]
                        ]
                    ]

                mEdge = do
                  (_, targetContent, _) <- M.lookup edgeKey graph
                  guard . not $ null targetContent
                  Just $
                    quoted (keyStr key) <> ":" <> BSB.wordDec idx
                    <> " -> " <> quoted (keyStr edgeKey)
                    <> " [tooltip=\" \" colorscheme=set28 color=" <> color <> "];"

             in ( edgeEl : cs
                , maybe id (:) mEdge es
                , nextColors
                , M.insert edgeKey (key, color) colorMap
                )
        doEntry ac _ = ac

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

edgeColors :: [BSB.Builder]
edgeColors = BSB.intDec <$> [1..8 :: Int]

pprSrcCodeLoc :: SrcCodeLoc -> BSB.Builder
pprSrcCodeLoc loc
  = BSB.byteString (srcMod loc) <> ":"
 <> BSB.intDec (srcLine loc) <> ":"
 <> BSB.intDec (srcCol loc)
