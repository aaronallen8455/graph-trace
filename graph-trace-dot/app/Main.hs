import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import qualified Data.List as List
import qualified System.Directory as Dir
import           System.Environment
import           System.IO

import           Graph.Trace.Dot (buildTree, buildNexus, graphToDot, parseLogEntries)

main :: IO ()
main = do
  args <- getArgs

  let isFlag arg = "--" `List.isPrefixOf` arg
      (flags, fileArgs) = span isFlag args
      nexusFlag = "--nexus" `List.elem` flags

  traceFiles <- case fileArgs of
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

    let tree = buildTree logContents
        dotFileContent
          | nexusFlag = graphToDot $ buildNexus tree
          | otherwise = graphToDot tree
        fileName = (<> ".dot")
                 $ if ".trace" `List.isSuffixOf` traceFile
                      then reverse . drop 6 $ reverse traceFile
                      else traceFile

    withFile fileName WriteMode $ \h -> do
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      BSB.hPutBuilder h dotFileContent
