{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graph.Trace.Internal.RuntimeRep
  ( LPId(..)
  ) where

import           GHC.Exts

-- | Levity polymorphic id function. Doesn't cover all runtime reps, in
-- particular unboxed products and sums.
class LPId (r :: RuntimeRep) where
  lpId :: forall (a :: TYPE r). a -> a

instance LPId LiftedRep where
  lpId = id
instance LPId UnliftedRep where
  lpId x = x
instance LPId 'IntRep where
  lpId x = x
instance LPId 'Int8Rep where
  lpId x = x
instance LPId 'Int16Rep where
  lpId x = x
instance LPId 'Int32Rep where
  lpId x = x
instance LPId 'Int64Rep where
  lpId x = x
instance LPId 'WordRep where
  lpId x = x
instance LPId 'Word8Rep where
  lpId x = x
instance LPId 'Word16Rep where
  lpId x = x
instance LPId 'Word32Rep where
  lpId x = x
instance LPId 'Word64Rep where
  lpId x = x
instance LPId 'AddrRep where
  lpId x = x
instance LPId 'FloatRep where
  lpId x = x
instance LPId 'DoubleRep where
  lpId x = x
