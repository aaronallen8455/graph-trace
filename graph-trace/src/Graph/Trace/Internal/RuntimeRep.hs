{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graph.Trace.Internal.RuntimeRep
  ( Lev
  ) where

import           GHC.Exts

class DummyConstraint
instance DummyConstraint

-- | Allows for a levity polymorphic value to be used in an argument position.
-- This trick was taken from Ed Kmett's `unboxed` library.
type Lev (a :: TYPE rep) = DummyConstraint => a
