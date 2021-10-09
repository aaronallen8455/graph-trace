{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module DebugPlugin.Test where

import           Data.Kind
import           GHC.TypeLits

type Debug (str :: Symbol) = (?x :: String)

test :: (Debug "yo", Num r) => r -> IO String
test _ = do
  x <- getLine
  let ?x = x
  pure ?x

