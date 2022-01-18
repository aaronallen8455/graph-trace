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
  , makeInstancesForRep
  , allRuntimeReps
  ) where

import           Control.Monad
import           Data.Traversable
import           GHC.Exts
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types (Multiplicity(..))
#endif
import           Language.Haskell.TH

-- | Levity polymorphic id function. Doesn't cover all runtime reps, in
-- particular unboxed products and sums. Handles linearity as well.
#if MIN_VERSION_ghc(9,0,0)
class LPId (r :: RuntimeRep) (m :: Multiplicity) where
  lpId :: forall (a :: TYPE r). a %m -> a
#else
class LPId (r :: RuntimeRep) where
  lpId :: forall (a :: TYPE r). a -> a
#endif

-- | A splice for generating instances for a given RuntimeRep
makeInstancesForRep :: Q Type -> Q [InstanceDec]
makeInstancesForRep rep = do
  let instTypes =
#if MIN_VERSION_ghc(9,0,0)
        [ [t| LPId |] `appT` rep `appT` [t| One |]
        , [t| LPId |] `appT` rep `appT` [t| Many |]
        ]
#else
        [ [t| LPId |] `appT` rep ]
#endif
  for instTypes $ \instType -> do
    x <- newName "x"
    instanceD (pure []) instType
      [ funD 'lpId [clause [varP x] (normalB $ varE x) []] ]

-- | RuntimeReps to generate instances for
runtimeReps :: [Q Type]
runtimeReps =
  [ [t| LiftedRep   |]
  , [t| UnliftedRep |]
  , [t| IntRep      |]
  , [t| Int8Rep     |]
  , [t| Int16Rep    |]
  , [t| Int32Rep    |]
  , [t| Int64Rep    |]
  , [t| WordRep     |]
  , [t| Word8Rep    |]
  , [t| Word16Rep   |]
  , [t| Word32Rep   |]
  , [t| Word64Rep   |]
  , [t| AddrRep     |]
  , [t| FloatRep    |]
  , [t| DoubleRep   |]
  ]

tupleReps :: [[Q Type]]
tupleReps = do
  len <- [0..2]
  replicateM len runtimeReps

unboxedTupleReps :: [Q Type]
unboxedTupleReps = map go tupleReps where
  go tupleRep = do
    tys <- sequence tupleRep
    let list = foldr (AppT . AppT PromotedConsT) PromotedNilT tys
    conT 'TupleRep `appT` pure list

unboxedSumReps :: [Q Type]
unboxedSumReps = map go tupleReps where
  go tupleRep = do
    tys <- sequence tupleRep
    let list = foldr (AppT . AppT PromotedConsT) PromotedNilT tys
    conT 'SumRep `appT` pure list

-- Does not include SIMD vectors b/c they are platform dependent
allRuntimeReps :: [Q Type]
allRuntimeReps = runtimeReps <> unboxedTupleReps <> unboxedSumReps
