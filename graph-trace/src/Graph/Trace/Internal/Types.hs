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
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           GHC.TypeLits
import qualified Language.Haskell.TH.Syntax as TH

data Propagation
  = Mute -- ^ Does not output traces, overrides other options
  | Inert -- ^ Does not output traces, doesn't override other options
  | Shallow -- ^ Outputs traces for current scope, but does not propagate
  | Deep -- ^ Outputs traces and propagates to descendents
  deriving (Eq, Show, TH.Lift)

data DebugContext =
  DC { previousTag :: Maybe DebugTag
     , currentTag :: DebugTag
     , propagation :: Propagation
     }

type DebugIP = (?_debug_ip :: Maybe DebugContext)
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
      DebugTag -- ^ Current context
      (Maybe DebugTag) -- ^ caller's context
  | TraceEvent
      DebugTag
      MessageContent

eventToLogStr :: Event -> BSL.ByteString
eventToLogStr (EntryEvent current mPrevious) =
  BSL8.intercalate "|"
    [ "entry"
    , keyStr current
    , BSL8.pack . show $ invocationId current
    , maybe "" keyStr mPrevious
    , maybe "" (BSL8.pack . show . invocationId) mPrevious
    ]
eventToLogStr (TraceEvent current message) =
  BSL8.intercalate "|"
    [ "trace"
    , keyStr current
    , BSL8.pack . show $ invocationId current
    , message
    ]

keyStr :: DebugTag -> BSL.ByteString
keyStr
  = either
      BSL8.pack
      BSL8.pack
  . debugKey
