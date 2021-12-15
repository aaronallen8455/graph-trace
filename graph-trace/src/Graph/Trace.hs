{-# LANGUAGE RecordWildCards #-}
module Graph.Trace
  ( plugin
  , module DT
  , module Trace
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.CPS
import qualified Data.Generics as Syb
import qualified Data.Set as S

import           Graph.Trace.Internal.Predicates (addConstraintToSig, removeConstraints)
import qualified Graph.Trace.Internal.GhcFacade as Ghc
import           Graph.Trace.Internal.Instrument (modifyClsInstDecl, modifyTyClDecl, modifyValBinds)
import           Graph.Trace.Internal.Solver (tcPlugin)
import           Graph.Trace.Internal.Types as DT
import           Graph.Trace.Internal.Trace as Trace

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.pluginRecompile = Ghc.purePlugin
    , Ghc.tcPlugin = \_ -> Just tcPlugin
    , Ghc.renamedResultAction = renamedResultAction
    }

findImportedModule :: String -> Ghc.TcM Ghc.Module
findImportedModule moduleName = do
  hscEnv <- Ghc.getTopEnv
  result <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName moduleName) Nothing
  case result of
    Ghc.Found _ m -> pure m
    _ -> error $ "unable to find module: " <> moduleName

warnAboutOptimizations :: Ghc.TcM ()
warnAboutOptimizations = do
  generalFlags <- Ghc.generalFlags <$> Ghc.getDynFlags
  when (Ghc.enumSetMember Ghc.Opt_FullLaziness generalFlags) .
    liftIO $ putStrLn " * Full laziness is enabled: it's generally recommended to disable this optimization when using graph-trace. Use the -fno-full-laziness GHC option to disable it."
  when (Ghc.enumSetMember Ghc.Opt_CSE generalFlags) .
    liftIO $ putStrLn " * Common sub-expression elimination is enabled: it's generally recommended to disable this optimization when using graph-trace. Use the -fno-cse GHC option to disable it."

isMonomorphismRestrictionOn :: Ghc.TcM Bool
isMonomorphismRestrictionOn =
  Ghc.xopt Ghc.MonomorphismRestriction <$> Ghc.getDynFlags

renamedResultAction
  :: [Ghc.CommandLineOption]
  -> Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction cmdLineOptions tcGblEnv
    hsGroup@Ghc.HsGroup{Ghc.hs_valds = Ghc.XValBindsLR{}}
    = do
  warnAboutOptimizations

  debugTypesModule <- findImportedModule "Graph.Trace.Internal.Types"
  debugTraceModule <- findImportedModule "Graph.Trace.Internal.Trace"

  debugMutePredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugMute")
  debugDeepPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugDeep")
  debugDeepKeyPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugDeepKey")
  debugPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "Debug")
  debugKeyPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugKey")
  debugInertPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugInert")
  entryName <- Ghc.lookupOrig debugTraceModule (Ghc.mkVarOcc "entry")
  debugContextName <- Ghc.lookupOrig debugTypesModule (Ghc.mkTcOcc "DebugContext")

  let debugNames = DebugNames{..}

  -- If the "debug-all" option is passed, add the Debug predicate to all
  -- function signatures.
  let debugAllFlag = "debug-all" `elem` cmdLineOptions
      (hsGroup'@Ghc.HsGroup
        { Ghc.hs_valds = valBinds --Ghc.XValBindsLR (Ghc.NValBinds binds sigs)
        , Ghc.hs_tyclds = tyClGroups
        }, nameMap) = runWriter
          $ Syb.mkM (addConstraintToSig debugNames debugAllFlag)
              `Syb.everywhereM` hsGroup

  -- process value bindings
  (valBinds', patBindNames) <- (`evalStateT` S.empty) . runWriterT $
      Syb.mkM (modifyValBinds debugNames nameMap)
    `Syb.everywhereM`
      valBinds

  -- process type class decls and instances
  -- TODO Only need to traverse with modifyValBinds. Others are not applied deeply
  (tyClGroups', tyClPatBindNames) <- (`evalStateT` S.empty) . runWriterT $
      Syb.mkM (modifyClsInstDecl debugNames nameMap)
    `Syb.extM`
      modifyTyClDecl debugNames nameMap
    `Syb.extM`
      modifyValBinds debugNames nameMap
    `Syb.everywhereM`
      tyClGroups

  mmrOn <- isMonomorphismRestrictionOn

  -- remove predicates from signatures for pattern bound ids if monomorphism
  -- restriction is on, otherwise compilation will fail.
  let (valBinds'', tyClGroups'') =
        if mmrOn
           then ( removeConstraints debugNames patBindNames valBinds'
                , removeConstraints debugNames tyClPatBindNames tyClGroups'
                )
           else (valBinds', tyClGroups')

  pure ( tcGblEnv
       , hsGroup' { Ghc.hs_valds = valBinds''
                  , Ghc.hs_tyclds = tyClGroups''
                  }
       )

renamedResultAction _ tcGblEnv group = pure (tcGblEnv, group)
