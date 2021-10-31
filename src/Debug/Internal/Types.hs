{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
module Debug.Internal.Types
  ( DebugTag(..)
  , DebugIPTy
  , Debug
  , DebugKey
  , Event(..)
  , eventToLogStr
  , FunName
  , UserKey
  ) where

import qualified Data.List as List
import           GHC.TypeLits

type DebugIPTy = (Maybe DebugTag, DebugTag)
type Debug = (?_debug_ip :: Maybe DebugIPTy) -- (DebugKey key, ?_debug_ip :: String)
type DebugKey (key :: Symbol) = (?_debug_ip :: Maybe DebugIPTy) -- (DebugKey key, ?_debug_ip :: String)
type FunName = String
type UserKey = String

data DebugTag =
  DT { invocationId :: {-# UNPACK #-} !Word -- a unique identifier for a particular invocation of a function
     , debugKey :: Either FunName UserKey -- !(Maybe String)
         -- The name of the function containing the current execution context
     }

data Event
  = EntryEvent
      DebugTag -- ^ Current context
      (Maybe DebugTag) -- ^ caller's context
  | TraceEvent
      DebugTag
      String

eventToLogStr :: Event -> String
eventToLogStr (EntryEvent current mPrevious) =
  List.intercalate "|"
    [ "entry"
    , keyStr current
    , show $ invocationId current
    , maybe "" keyStr mPrevious
    , maybe "" (show . invocationId) mPrevious
    ]
eventToLogStr (TraceEvent current message) =
  List.intercalate "|"
    [ "trace"
    , keyStr current
    , show $ invocationId current
    , message
    ]

keyStr :: DebugTag -> String
keyStr
  = either
      id
      id
  . debugKey
