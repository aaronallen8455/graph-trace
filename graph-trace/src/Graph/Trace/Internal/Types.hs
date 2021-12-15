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
  , DebugMute
  , DebugDeep
  , DebugDeepKey
  , Debug
  , DebugKey
  , DebugInert
  , Event(..)
  , eventToLogStr
  , FunName
  , UserKey
  , SrcModule
  , SrcLine
  , SrcCol
  , callStackToCallSite
  , DebugNames(..)
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
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
type DebugMute = DebugIP
type DebugDeep = DebugIP
type DebugDeepKey (key :: Symbol) = DebugIP
type Debug = DebugIP
type DebugKey (key :: Symbol) = DebugIP
type DebugInert = DebugIP
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

-- | Serialize an Event. The § character is used as both a separator and
-- terminator. Don't use this character in trace messages, it will break!
eventToLogStr :: Event -> BSL.ByteString
eventToLogStr (EntryEvent current mPrevious mDefSite mCallSite) =
  BSL8.intercalate "§"
    [ "entry"
    , keyStr current
    , BSL8.pack . show $ invocationId current
    , maybe "" keyStr mPrevious
    , maybe "" (BSL8.pack . show . invocationId) mPrevious
    , srcCodeLocToLogStr mDefSite
    , srcCodeLocToLogStr mCallSite
    ] <> "§"
eventToLogStr (TraceEvent current message mCallSite) =
  BSL8.intercalate "§"
    [ "trace"
    , keyStr current
    , BSL8.pack . show $ invocationId current
    , message
    , srcCodeLocToLogStr mCallSite
    ] <> "§"

srcCodeLocToLogStr :: Maybe SrcCodeLoc -> BSL.ByteString
srcCodeLocToLogStr mLoc =
  BSL8.intercalate "§"
    [ foldMap (BSL8.pack . srcModule) mLoc
    , foldMap (BSL8.pack . show . srcLine) mLoc
    , foldMap (BSL8.pack . show . srcCol) mLoc
    ]

keyStr :: DebugTag -> BSL.ByteString
keyStr
  = either
      BSL8.pack
      BSL8.pack
  . debugKey

data DebugNames =
  DebugNames
    { debugMutePredName :: Ghc.Name
    , debugDeepPredName :: Ghc.Name
    , debugDeepKeyPredName :: Ghc.Name
    , debugPredName :: Ghc.Name
    , debugKeyPredName :: Ghc.Name
    , debugInertPredName :: Ghc.Name
    , entryName :: Ghc.Name
    , debugContextName :: Ghc.Name
    }
