import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import qualified Data.List as List
import qualified System.Directory as Dir
import           System.Environment
import           System.IO

import           Graph.Trace.Dot (buildGraph, graphToDot, parseLogEntries)

main :: IO ()
main = do
  args <- getArgs

  traceFiles <- case args of
    [] -> do
      contents <- Dir.listDirectory =<< Dir.getCurrentDirectory
      let isTraceFile = (".trace" `List.isSuffixOf`)
      pure $ filter isTraceFile contents
    xs -> pure xs

  for_ traceFiles $ \traceFile -> do
    logContents
      <- either (\err -> fail $ "Failed parsing trace file: " <> err) id
       . parseLogEntries
     <$> BSL.readFile traceFile

    let dotFileContent = graphToDot $ buildGraph logContents
        fileName = (<> ".dot")
                 $ if ".trace" `List.isSuffixOf` traceFile
                      then reverse . drop 6 $ reverse traceFile
                      else traceFile

    withFile fileName WriteMode $ \h -> do
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      BSB.hPutBuilder h dotFileContent
