{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Graph.Trace.Internal.TH
  ( makeInstancesForRep
  , allRuntimeReps
  ) where

import           Control.Monad
import           Data.Traversable
import           GHC.Exts
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types (Multiplicity(..))
#endif
import           Language.Haskell.TH

-- | A splice for generating instances for a given RuntimeRep
makeInstancesForRep :: Name -> Name -> Q Type -> Q [InstanceDec]
makeInstancesForRep cls meth rep = do
  let instTypes =
#if MIN_VERSION_ghc(9,0,0)
        [ conT cls `appT` rep `appT` [t| One |]
        , conT cls `appT` rep `appT` [t| Many |]
        ]
#else
        [ conT cls `appT` rep ]
#endif
  for instTypes $ \instType -> do
    x <- newName "x"
    instanceD (pure []) instType
      [ funD meth [clause [varP x] (normalB $ varE x) []] ]

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
