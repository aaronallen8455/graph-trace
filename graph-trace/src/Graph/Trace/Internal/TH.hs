{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
module Graph.Trace.Internal.TH where

import           Graph.Trace.Internal.RuntimeRep

$(concat <$> traverse makeInstancesForRep allRuntimeReps)
