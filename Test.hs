{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module DebugPlugin.Test where

import           Data.Kind
import           GHC.TypeLits

type Debug (str :: Symbol) = (?x :: String)

test :: Debug "yo" => String
test = let ?x = newIP in
  do ?x
 where
  newIP = ?x <> "test"

