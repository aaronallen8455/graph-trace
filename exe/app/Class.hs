module Class where

import           Graph.Trace

class Show a => Classy a where
  classy :: Debug => a -> String

  deff :: Debug => a -> String
  deff = show

class Show a => Classier a where
  classier :: a -> String
