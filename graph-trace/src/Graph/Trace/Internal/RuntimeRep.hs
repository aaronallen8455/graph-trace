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

-- | Levity polymorphic id function. Doesn't cover all runtime reps, in
-- particular unboxed products and sums. Handles linearity as well.
#if MIN_VERSION_ghc(9,0,0)
class LPId (r :: RuntimeRep) (m :: Multiplicity) where
  lpId :: forall (a :: TYPE r). a %m -> a
#else
class LPId (r :: RuntimeRep) where
  lpId :: forall (a :: TYPE r). a -> a
#endif

#if MIN_VERSION_ghc(9,0,0)
instance LPId LiftedRep One where
  lpId x = x
instance LPId LiftedRep Many where
  lpId x = x
instance LPId UnliftedRep One where
  lpId x = x
instance LPId UnliftedRep Many where
  lpId x = x
instance LPId IntRep One where
  lpId x = x
instance LPId IntRep Many where
  lpId x = x
instance LPId Int8Rep One where
  lpId x = x
instance LPId Int8Rep Many where
  lpId x = x
instance LPId Int16Rep One where
  lpId x = x
instance LPId Int16Rep Many where
  lpId x = x
instance LPId Int32Rep One where
  lpId x = x
instance LPId Int32Rep Many where
  lpId x = x
instance LPId Int64Rep One where
  lpId x = x
instance LPId Int64Rep Many where
  lpId x = x
instance LPId WordRep One where
  lpId x = x
instance LPId WordRep Many where
  lpId x = x
instance LPId Word8Rep One where
  lpId x = x
instance LPId Word8Rep Many where
  lpId x = x
instance LPId Word16Rep One where
  lpId x = x
instance LPId Word16Rep Many where
  lpId x = x
instance LPId Word32Rep One where
  lpId x = x
instance LPId Word32Rep Many where
  lpId x = x
instance LPId Word64Rep One where
  lpId x = x
instance LPId Word64Rep Many where
  lpId x = x
instance LPId AddrRep One where
  lpId x = x
instance LPId AddrRep Many where
  lpId x = x
instance LPId FloatRep One where
  lpId x = x
instance LPId FloatRep Many where
  lpId x = x
instance LPId DoubleRep One where
  lpId x = x
instance LPId DoubleRep Many where
  lpId x = x
#else
instance LPId LiftedRep where
  lpId = id
instance LPId UnliftedRep where
  lpId x = x
instance LPId IntRep where
  lpId x = x
instance LPId Int8Rep where
  lpId x = x
instance LPId Int16Rep where
  lpId x = x
instance LPId Int32Rep where
  lpId x = x
instance LPId Int64Rep where
  lpId x = x
instance LPId WordRep where
  lpId x = x
instance LPId Word8Rep where
  lpId x = x
instance LPId Word16Rep where
  lpId x = x
instance LPId Word32Rep where
  lpId x = x
instance LPId Word64Rep where
  lpId x = x
instance LPId AddrRep where
  lpId x = x
instance LPId FloatRep where
  lpId x = x
instance LPId DoubleRep where
  lpId x = x
#endif
