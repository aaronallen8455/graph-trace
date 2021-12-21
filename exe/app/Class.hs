{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
module Class where

import           Graph.Trace

class Show a => Classy a where
  classy :: a -> String

  deff :: Trace => a -> String
  deff = show

class Show a => Classier a where
  classier :: a -> String
