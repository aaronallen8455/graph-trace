{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Debug.Internal.Trace
  ( trace
  , traceId
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  , entry
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           Debug.Internal.Types

trace :: (?_debug_ip :: Maybe DebugIPTy) => String -> a -> a
-- forcing msg is required here since the file MVar could be entagled with it
trace !msg x =
  case ?_debug_ip of
    Nothing -> x
    Just ip -> unsafePerformIO $ do
      withMVar fileLock $ \h -> do
        let ev = TraceEvent (snd ip) msg
        hPutStrLn h $ eventToLogStr ev
      pure x
{-# NOINLINE trace  #-}

traceId :: (?_debug_ip :: Maybe DebugIPTy) => String -> String
traceId = join trace

traceShow :: (?_debug_ip :: Maybe DebugIPTy) => Show a => a -> b -> b
traceShow = trace . show

traceShowId :: (?_debug_ip :: Maybe DebugIPTy) => Show a => a -> a
traceShowId = join traceShow

traceM :: (?_debug_ip :: Maybe DebugIPTy, Applicative f) => String -> f ()
traceM x = trace x $ pure ()

traceShowM :: (?_debug_ip :: Maybe DebugIPTy, Applicative f, Show a) => a -> f ()
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
entry :: (?_debug_ip :: Maybe DebugIPTy) => a -> a
entry x =
  case ?_debug_ip of
    Nothing -> x
    Just ip -> unsafePerformIO $ do
      withMVar fileLock $ \h -> do
        let ev = EntryEvent (snd ip) (fst ip)
        hPutStrLn h $ eventToLogStr ev
      pure x
{-# NOINLINE entry  #-}

