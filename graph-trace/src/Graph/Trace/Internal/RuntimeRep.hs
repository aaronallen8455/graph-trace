{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if MIN_VERSION_ghc(9,0,0)
{-# LANGUAGE LinearTypes #-}
#endif
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graph.Trace.Internal.RuntimeRep
  ( LPId(..)
  ) where

import           GHC.Exts
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types (Multiplicity(..))
#endif

import           Graph.Trace.Internal.TH (allRuntimeReps, makeInstancesForRep)

-- | Levity polymorphic id function. Doesn't cover all runtime reps, in
-- particular unboxed products and sums with more than 2 elements. Handles
-- linearity as well.
#if MIN_VERSION_ghc(9,0,0)
class LPId (r :: RuntimeRep) (m :: Multiplicity) where
  lpId :: forall (a :: TYPE r). a %m -> a
#else
class LPId (r :: RuntimeRep) where
  lpId :: forall (a :: TYPE r). a -> a
#endif


$(concat <$> traverse (makeInstancesForRep ''LPId 'lpId) allRuntimeReps)
