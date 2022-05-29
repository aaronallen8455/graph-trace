{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
module Graph.Trace.Internal.Types
  ( DebugTag(..)
  , DebugContext(..)
  , Propagation(..)
  , SrcCodeLoc(..)
  , DefinitionSite
  , CallSite
  , DebugIP
  , TraceMute
  , TraceDeep
  , TraceDeepKey
  , Trace
  , TraceKey
  , TraceInert
  , Event(..)
  , eventToLogStr
  , FunName
  , UserKey
  , SrcModule
  , SrcLine
  , SrcCol
  , callStackToCallSite
  , DebugNames(..)
  , Builder(..)
  ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.String (IsString(..))
import qualified Mason.Builder as Mason
import           GHC.Stack
import           GHC.TypeLits
import qualified Language.Haskell.TH.Syntax as TH

import qualified Graph.Trace.Internal.GhcFacade as Ghc

data Propagation
  = Mute -- ^ Does not output traces, overrides other options
  | Inert -- ^ Does not output traces, doesn't override other options
  | Shallow -- ^ Outputs traces for current scope, but does not propagate
  | Deep -- ^ Outputs traces and propagates to descendents
  deriving (Eq, Show, TH.Lift)

data DebugContext =
  DC { previousTag :: !(Maybe DebugTag)
     , currentTag :: {-# UNPACK #-} !DebugTag
     , propagation :: !Propagation
     , definitionSite :: !(Maybe DefinitionSite)
     }

data SrcCodeLoc =
  SrcCodeLoc
    { srcModule :: !SrcModule
    , srcLine :: !SrcLine
    , srcCol :: !SrcCol
    } deriving TH.Lift

type SrcModule = String
type SrcLine = Int
type SrcCol = Int

type DefinitionSite = SrcCodeLoc
type CallSite = SrcCodeLoc

type DebugIP = (?_debug_ip :: Maybe DebugContext, HasCallStack)
type TraceMute = DebugIP
type TraceDeep = DebugIP
type TraceDeepKey (key :: Symbol) = DebugIP
type Trace = DebugIP
type TraceKey (key :: Symbol) = DebugIP
type TraceInert = DebugIP
-- These are String because they need to be lifted into TH expressions
type FunName = String
type UserKey = String
type MessageContent = BSL.ByteString

data DebugTag =
  DT { invocationId :: {-# UNPACK #-} !Word -- a unique identifier for a particular invocation of a function
     , debugKey :: Either FunName UserKey
         -- The name of the function containing the current execution context
     }

data Event
  = EntryEvent
      !DebugTag -- ^ Current context
      !(Maybe DebugTag) -- ^ caller's context
      !(Maybe DefinitionSite)
      !(Maybe CallSite)
  | TraceEvent
      !DebugTag
      !MessageContent
      !(Maybe CallSite)

callStackToCallSite :: CallStack -> Maybe CallSite
callStackToCallSite cs =
  case getCallStack cs of
    (_, srcLoc) : _ ->
      Just SrcCodeLoc
        { srcModule = srcLocFile srcLoc
        , srcLine = srcLocStartLine srcLoc
        , srcCol = srcLocStartCol srcLoc
        }
    _ -> Nothing

sep :: Builder
sep = MkB $ Mason.char8 '§'

-- | Serialize an Event. The § character is used as both a separator and
-- terminator. Don't use this character in trace messages, it will break!
eventToLogStr :: Event -> Builder
eventToLogStr (EntryEvent current mPrevious mDefSite mCallSite)
   = stringUtf8 "entry" <> sep
  <> keyStr current <> sep
  <> wordDec (invocationId current) <> sep
  <> foldMap keyStr mPrevious <> sep
  <> foldMap (wordDec . invocationId) mPrevious <> sep
  <> srcCodeLocToLogStr mDefSite <> sep
  <> srcCodeLocToLogStr mCallSite <> sep
eventToLogStr (TraceEvent current message mCallSite)
   = stringUtf8 "trace" <> sep
  <> keyStr current <> sep
  <> wordDec (invocationId current) <> sep
  <> lazyByteString message <> sep
  <> srcCodeLocToLogStr mCallSite <> sep

srcCodeLocToLogStr :: Maybe SrcCodeLoc -> Builder
srcCodeLocToLogStr mLoc
   = foldMap (stringUtf8 . srcModule) mLoc <> sep
  <> foldMap (intDec . srcLine) mLoc <> sep
  <> foldMap (intDec . srcCol) mLoc

keyStr :: DebugTag -> Builder
keyStr
  = stringUtf8
  . either
      id
      id
  . debugKey

data DebugNames =
  DebugNames
    { traceMutePredName :: Ghc.Name
    , traceDeepPredName :: Ghc.Name
    , traceDeepKeyPredName :: Ghc.Name
    , tracePredName :: Ghc.Name
    , traceKeyPredName :: Ghc.Name
    , traceInertPredName :: Ghc.Name
    , entryName :: Ghc.Name
    , debugContextName :: Ghc.Name
    }

-- A wrapper to avoid impredicative types
newtype Builder =
  MkB { runBuilder :: forall s. Mason.Buildable s => Mason.BuilderFor s }

instance Semigroup Builder where
  a <> b = MkB $ runBuilder a <> runBuilder b

instance Monoid Builder where
  mempty = MkB mempty

instance IsString Builder where
  fromString s = MkB $ fromString s

intDec :: Int -> Builder
intDec i = MkB $ Mason.intDec i

wordDec :: Word -> Builder
wordDec w = MkB $ Mason.wordDec w

stringUtf8 :: String -> Builder
stringUtf8 str = MkB $ Mason.stringUtf8 str

lazyByteString :: BSL.ByteString -> Builder
lazyByteString bs = MkB $ Mason.lazyByteString bs
