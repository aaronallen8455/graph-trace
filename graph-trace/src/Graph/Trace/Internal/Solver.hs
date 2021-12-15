{-# LANGUAGE OverloadedStrings #-}
module Graph.Trace.Internal.Solver
  ( tcPlugin
  ) where

import qualified Graph.Trace.Internal.GhcFacade as Ghc

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = pure ()
    , Ghc.tcPluginStop = \_ -> pure ()
    , Ghc.tcPluginSolve = const tcPluginSolver
    }

debuggerIpKey :: Ghc.FastString
debuggerIpKey = "_debug_ip"

isDebuggerIpCt :: Ghc.Ct -> Bool
isDebuggerIpCt ct@Ghc.CDictCan{}
  | Ghc.className (Ghc.cc_class ct) == Ghc.ipClassName
  , ty : _ <- Ghc.cc_tyargs ct
  , Just ipKey <- Ghc.isStrLitTy ty
  , ipKey == debuggerIpKey
  = True
isDebuggerIpCt _ = False

tcPluginSolver :: Ghc.TcPluginSolver
tcPluginSolver _ [] wanted = do
  case filter isDebuggerIpCt wanted of
    [w]
      | Ghc.IPOccOrigin _ <- Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
      -> do
        -- This occurs when the IP constraint is satisfied but a wanted still
        -- gets emitted for the a use site of the IP variable (why?).
        -- We don't want to touch this constraint because the value for the IP
        -- should be inherited from the context.
        pure $ Ghc.TcPluginOk [] []
      | otherwise
      -> do
           -- This occurs when the IP constraint is not satisfiable by the context.
           -- Here we want to manually construct a value with which to satisfy it.
           let expr = Ghc.mkNothingExpr Ghc.anyTy
           pure $ Ghc.TcPluginOk [(Ghc.EvExpr expr, w)] []
    _ -> pure $ Ghc.TcPluginOk [] []
tcPluginSolver _ _ _ = pure $ Ghc.TcPluginOk [] []
