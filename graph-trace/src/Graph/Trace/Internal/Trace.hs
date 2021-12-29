{-# LANGUAGE CPP #-}
# if MIN_VERSION_ghc(9,0,0)
{-# LANGUAGE LinearTypes #-}
# endif
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           GHC.Exts
import           GHC.Stack (callStack, popCallStack)
import           System.Environment (getProgName, lookupEnv)
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           Graph.Trace.Internal.RuntimeRep (LPId(..))
import           Graph.Trace.Internal.Types

mkTraceEvent :: DebugIP => String -> Maybe Event
mkTraceEvent !msg = do
  ip <- ?_debug_ip
  guard . not $ omitTraces (propagation ip)
  pure $
    TraceEvent
      (currentTag ip)
      (BSL8.pack msg)
      (callStackToCallSite . popCallStack $ popCallStack callStack)

writeEventToLog :: Event -> IO ()
-- forcing msg is required here since the file MVar could be entagled with it
writeEventToLog event = seq fileLock $
  withMVar fileLock $ \h ->
    BSL.hPut h . (<> "\n") $ eventToLogStr event

unsafeWriteTrace :: DebugIP => String -> a -> a
unsafeWriteTrace !msg thing =
  unsafePerformIO $ do
    case mkTraceEvent msg of
      Nothing -> pure ()
      Just event -> writeEventToLog event
    pure thing
{-# NOINLINE unsafeWriteTrace  #-}

trace :: DebugIP => String -> a -> a
trace = unsafeWriteTrace
{-# NOINLINE trace  #-}

traceId :: DebugIP => String -> String
traceId = join unsafeWriteTrace

traceShow :: (DebugIP, Show a) => a -> b -> b
traceShow = unsafeWriteTrace . show

traceShowId :: (DebugIP, Show a) => a -> a
traceShowId = join (unsafeWriteTrace . show)

traceM :: (Applicative f, DebugIP) => String -> f ()
traceM x = unsafeWriteTrace x $ pure ()

traceShowM :: (Applicative f, Show a, DebugIP) => a -> f ()
traceShowM x = unsafeWriteTrace (show x) $ pure ()

-- | Serializes access to the debug log file
fileLock :: MVar Handle
fileLock = unsafePerformIO $ do
  -- check for env variable with file name
  mOverrideFileName <- lookupEnv "GRAPH_TRACE_FILENAME"
  logFilePath <-
    case mOverrideFileName of
      Nothing -> do
        progName <- getProgName
        pure $ progName <> ".trace"
      Just n -> pure n
  h <- openFile logFilePath AppendMode
  hSetBuffering h NoBuffering
  newMVar h
{-# NOINLINE fileLock  #-}

-- | Emits a message to the log signaling a function invocation
entry
#if MIN_VERSION_ghc(9,0,0)
  :: forall rep m (a :: TYPE rep). (DebugIP, LPId rep m)
  => a %m -> a
#else
  :: forall rep (a :: TYPE rep). (DebugIP, LPId rep)
  => a -> a
#endif
entry =
  case ?_debug_ip of
    Nothing -> lpId
    Just ip
      | omitTraces (propagation ip) -> lpId
      | otherwise ->
        let !() = unsafePerformIO $ do
              withMVar fileLock $ \h -> do
                let ev = EntryEvent
                           (currentTag ip)
                           (previousTag ip)
                           (definitionSite ip)
                           -- need to call popCallStack here to get actual call site
                           (callStackToCallSite $ popCallStack callStack)
                BSL.hPut h . (<> "\n") $ eventToLogStr ev
         in lpId
{-# NOINLINE entry  #-}

omitTraces :: Propagation -> Bool
omitTraces Mute = True
omitTraces Inert = True
omitTraces _ = False

-- TODO allow to apply a function to mute some specific thing
-- mute :: DebugIP => a -> a
