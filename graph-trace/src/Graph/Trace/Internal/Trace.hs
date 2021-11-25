{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Graph.Trace.Internal.Trace
  ( trace
  , traceId
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  , entry
  , omitTraces
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           Graph.Trace.Internal.Types

trace :: DebugIP => String -> a -> a
-- forcing msg is required here since the file MVar could be entagled with it
trace !msg x =
  case ?_debug_ip of
    Nothing -> x
    Just ip
      | omitTraces (propagation ip) -> x
      | otherwise ->
          unsafePerformIO $ do
          withMVar fileLock $ \h -> do
            let ev = TraceEvent (currentTag ip) (BSL8.pack msg)
            BSL.hPut h . (<> "\n") $ eventToLogStr ev
          pure x
{-# NOINLINE trace  #-}

traceId :: DebugIP => String -> String
traceId = join trace

traceShow :: DebugIP => Show a => a -> b -> b
traceShow = trace . show

traceShowId :: DebugIP => Show a => a -> a
traceShowId = join traceShow

traceM :: (Applicative f, DebugIP) => String -> f ()
traceM x = trace x $ pure ()

traceShowM :: (Applicative f, Show a, DebugIP) => a -> f ()
traceShowM = traceM . show

logFilePath :: FilePath
logFilePath = "debug_log.txt"

-- | Serializes access to the debug log file
fileLock :: MVar Handle
fileLock = unsafePerformIO $ do
  h <- openFile logFilePath AppendMode
  hSetBuffering h NoBuffering
  newMVar h
{-# NOINLINE fileLock  #-}

-- | Emits a message to the log signaling a function invocation
entry :: DebugIP => a -> a
entry x =
  case ?_debug_ip of
    Nothing -> x
    Just ip
      | omitTraces (propagation ip) -> x
      | otherwise -> unsafePerformIO $ do
          withMVar fileLock $ \h -> do
            let ev = EntryEvent (currentTag ip) (previousTag ip)
            BSL.hPut h . (<> "\n") $ eventToLogStr ev
          pure x
{-# NOINLINE entry  #-}

omitTraces :: Propagation -> Bool
omitTraces Mute = True
omitTraces Inert = True
omitTraces _ = False

-- TODO allow to apply a function to mute some specific thing
-- mute :: DebugIP => a -> a
