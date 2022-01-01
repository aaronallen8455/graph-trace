{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed (embedFile)
import           Data.Foldable (for_)
import qualified Data.List as List
import           Data.Maybe (isJust)
import qualified System.Directory as Dir
import           System.Environment
import           System.Exit (die)
import           System.IO
import qualified System.Process as Proc

import qualified Graph.Trace.Dot as Dot

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
       . Dot.parseLogEntries
     <$> BSL.readFile traceFile

    let dotFileContent = Dot.graphToDot $ Dot.buildGraph logContents
        fileName = (<> ".html")
                 $ if ".trace" `List.isSuffixOf` traceFile
                      then reverse . drop 6 $ reverse traceFile
                      else traceFile

        htmlHeader = $(embedFile "extras/header.html")
        htmlFooter = $(embedFile "extras/footer.html")

    dotExists <- isJust <$> Dir.findExecutable "dot"
    unless dotExists $ die "Error! Graphviz is not installed or not accessible"

    withFile fileName WriteMode $ \h -> do
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      BS.hPut h htmlHeader
      writeSvg h dotFileContent
      BS.hPut h htmlFooter

-- | Invoke @dot@ to produce an svg document and write to the file handle
writeSvg :: Handle -> BSB.Builder -> IO ()
writeSvg htmlFile dotContent =
  Proc.withCreateProcess (Proc.proc "dot" ["-Tsvg"])
      { Proc.std_in = Proc.CreatePipe
      , Proc.std_out = Proc.CreatePipe
      } go
  where
    go (Just stdIn) (Just stdOut) _ _ = do
      hSetBinaryMode stdIn True
      hSetBuffering stdIn (BlockBuffering Nothing)
      hSetBinaryMode stdOut True
      hSetBuffering stdOut (BlockBuffering Nothing)
      _ <- BSB.hPutBuilder stdIn dotContent
      hClose stdIn
      svg <- BSL.hGetContents stdOut
      BSL.hPut htmlFile svg
      hClose stdOut
    go _ _ _ _ = pure ()

