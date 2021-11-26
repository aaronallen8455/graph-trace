{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph.Trace
  ( plugin
  , module DT
  , module Trace
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer.CPS
import qualified Data.Generics as Syb
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           GHC.Magic (noinline)
import qualified Language.Haskell.TH as TH
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as Rand

import qualified Graph.Trace.Internal.GhcFacade as Ghc
import           Graph.Trace.Internal.Types
import qualified Graph.Trace.Internal.Types as DT
import           Graph.Trace.Internal.Trace as Trace

-- import qualified Debug.Trace as D

-- TODO If more than one application is running at once, will need to use
-- different names for log files. There may be a way to query what the name of
-- the running application is, otherwise it could be a plugin argument. Looks
-- like you can get the package name from the CallStack, so maybe that will
-- work? Probably not since the MVar is defined in the plugin package.

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.pluginRecompile = Ghc.purePlugin -- is this actually pure?
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
    liftIO $ putStrLn " * Full laziness is enabled: it's recommended to disable this optimization when using graph-trace. Use the -fno-full-laziness GHC option to disable it."
  when (Ghc.enumSetMember Ghc.Opt_CSE generalFlags) .
    liftIO $ putStrLn " * Common sub-expression elimination is enabled: it's recommended to disable this optimization when using graph-trace. Use the -fno-cse GHC option to disable it."

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
  valBinds' <- (`evalStateT` S.empty) $
      Syb.mkM (modifyValBinds debugNames nameMap)
    `Syb.everywhereM`
      valBinds

  -- process type class decls and instances
  -- TODO Only need to traverse with modifyValBinds. Other are not applied deeply
  tyClGroups' <- (`evalStateT` S.empty) $
      Syb.mkM (modifyClsInstDecl debugNames nameMap)
    `Syb.extM`
      modifyTyClDecl debugNames nameMap
    `Syb.extM`
      modifyValBinds debugNames nameMap
    `Syb.everywhereM`
      tyClGroups

  pure ( tcGblEnv
       , hsGroup' { Ghc.hs_valds = valBinds'
                  , Ghc.hs_tyclds = tyClGroups'
                  }
       )

renamedResultAction _ tcGblEnv group = pure (tcGblEnv, group)

data DebugNames =
  DebugNames
    { debugMutePredName :: Ghc.Name
    , debugDeepPredName :: Ghc.Name
    , debugDeepKeyPredName :: Ghc.Name
    , debugPredName :: Ghc.Name
    , debugKeyPredName :: Ghc.Name
    , debugInertPredName :: Ghc.Name
    , entryName :: Ghc.Name
    }

-- | Instrument a set of bindings given a Map containing the names of functions
-- that should be modified.
modifyBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.Name
  -> Ghc.LHsBinds Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.LHsBinds Ghc.GhcRn)
modifyBinds nameMap entryName =
  (traverse . traverse)
    (modifyBinding nameMap entryName)

-- | Instrument value bindings that have a signature with a debug pred.
modifyValBinds
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.NHsValBindsLR Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.NHsValBindsLR Ghc.GhcRn)
modifyValBinds debugNames nameMap (Ghc.NValBinds binds sigs) = do
  binds' <-
    (traverse . traverse)
      (modifyBinds nameMap (entryName debugNames))
      binds
  modify' (S.union $ M.keysSet nameMap)
  pure $ Ghc.NValBinds binds' sigs

-- | Instrument default method implementations in a type class declaration if
-- they contain a Debug pred.
modifyTyClDecl
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.TyClDecl Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.TyClDecl Ghc.GhcRn)
modifyTyClDecl debugNames nameMap
    cd@Ghc.ClassDecl { Ghc.tcdMeths = meths
                     } = do
  newMeths <- modifyBinds nameMap (entryName debugNames) meths
  pure cd { Ghc.tcdMeths = newMeths }
modifyTyClDecl _ _ x = pure x

-- | Instrument the method implementations in an type class instance if it has
-- a signature containing a debug pred.
modifyClsInstDecl
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.ClsInstDecl Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.ClsInstDecl Ghc.GhcRn)
modifyClsInstDecl debugNames nameMap
    inst@Ghc.ClsInstDecl{ Ghc.cid_binds = binds }
      = do
  newBinds <- modifyBinds nameMap (entryName debugNames) binds
  pure inst { Ghc.cid_binds = newBinds }
#if MIN_VERSION_ghc(9,0,0)
#else
modifyClsInstDecl _ _ x = pure x
#endif

-- | Matches on type signatures in order to add the constraint to them.
addConstraintToSig
  :: DebugNames
  -> Bool -- True <=> Debug all functions
  -> Ghc.Sig Ghc.GhcRn
  -> Writer (M.Map Ghc.Name (Maybe Ghc.FastString, Propagation))
            (Ghc.Sig Ghc.GhcRn)
addConstraintToSig debugNames debugAllFlag
  (Ghc.TypeSig x1 lNames (Ghc.HsWC x2 sig)) = do
    sig' <- addConstraintToSigType debugNames debugAllFlag (Ghc.unLoc <$> lNames) sig
    pure $ Ghc.TypeSig x1 lNames (Ghc.HsWC x2 sig')
addConstraintToSig debugNames debugAllFlag
  (Ghc.ClassOpSig x1 b lNames sig) = do
    sig' <- addConstraintToSigType debugNames debugAllFlag (Ghc.unLoc <$> lNames) sig
    pure $ Ghc.ClassOpSig x1 b lNames sig'
addConstraintToSig _ _ s = pure s

-- | Adds the 'Debug' constraint to a signature if it doesn't already have it
-- as the first constraint in the context.
addConstraintToSigType
  :: DebugNames
  -> Bool -- True <=> Debug all functions
  -> [Ghc.Name]
  -> Ghc.LHsSigType Ghc.GhcRn
  -> Writer (M.Map Ghc.Name (Maybe Ghc.FastString, Propagation))
            (Ghc.LHsSigType Ghc.GhcRn)
addConstraintToSigType debugNames debugAllFlag names sig@(Ghc.HsSig' t) = do
  sigBody <- traverse go t
  pure $ Ghc.setSigBody sigBody sig
    where
      prop = if debugAllFlag then Shallow else Inert
      predName =
        if debugAllFlag
           then debugPredName debugNames
           else debugInertPredName debugNames
      predTy = Ghc.noLocA'
             $ Ghc.HsTyVar Ghc.emptyEpAnn Ghc.NotPromoted
                 (Ghc.noLocA' predName)
      go ty =
        case ty of
          x@Ghc.HsForAllTy { Ghc.hst_body = body } -> do
            body' <- traverse go body
            pure $ x { Ghc.hst_body = body' }
          q@(Ghc.HsQualTy' x ctx body)
            | foundPred : _ <-
                mapMaybe (checkForDebugPred debugNames)
                  (Ghc.unLoc <$> foldMap Ghc.unLoc ctx)
            -> do tell (M.fromList $ names `zip` repeat foundPred)
                  pure q
            | otherwise -> do
                tell (M.fromList $ names `zip` repeat (Nothing, prop))
                pure $
                  Ghc.HsQualTy'
                    x
                    (Just $ maybe (Ghc.noLocA' [predTy])
                                  (fmap (predTy :))
                                  ctx
                    )
                    body
          _ -> do
              tell (M.fromList $ names `zip` repeat (Nothing, prop))
              pure $
                Ghc.HsQualTy'
                  Ghc.NoExtField
                  (Just $ Ghc.noLocA' [predTy])
                  (Ghc.noLocA' ty)
addConstraintToSigType _ _ _ x = pure x

checkForDebugPred
  :: DebugNames
  -> Ghc.HsType Ghc.GhcRn
  -> Maybe (Maybe Ghc.FastString, Propagation)
checkForDebugPred debugNames
    (Ghc.HsTyVar _ _ (Ghc.L _ name))
  | name == debugPredName debugNames = Just (Nothing, Shallow)
  | name == debugDeepPredName debugNames = Just (Nothing, Deep)
  | name == debugMutePredName debugNames = Just (Nothing, Mute)
  | name == debugInertPredName debugNames = Just (Nothing, Inert)
checkForDebugPred debugNames
    (Ghc.HsAppTy _ (Ghc.L _ (Ghc.HsTyVar _ _ (Ghc.L _ name))) (Ghc.L _ (Ghc.HsTyLit _ (Ghc.HsStrTy _ key))))
  | name == debugKeyPredName debugNames = Just (Just key, Shallow)
  | name == debugDeepKeyPredName debugNames = Just (Just key, Deep)
checkForDebugPred debugNames Ghc.HsForAllTy { Ghc.hst_body = Ghc.L _ ty }
  = checkForDebugPred debugNames ty
checkForDebugPred debugNames (Ghc.HsParTy _ (Ghc.L _ ty))
  = checkForDebugPred debugNames ty
checkForDebugPred _ _ = Nothing
-- need a case for nested QualTy?

-- | Instrument a binding if its name is in the Map.
modifyBinding
  :: M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.Name
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
modifyBinding nameMap entryName
  bnd@Ghc.FunBind { Ghc.fun_id = Ghc.L _ name
                  , Ghc.fun_matches = mg@(Ghc.MG _ alts _) }
    | Just (mUserKey, prop) <- M.lookup name nameMap
    = do
      let key = case mUserKey of
                  Nothing -> Left $ Ghc.getOccString name
                  Just k -> Right $ Ghc.unpackFS k

      whereBindExpr <- lift $ mkNewIpExpr key prop

      newAlts <-
        (traverse . traverse . traverse)
          (modifyMatch prop whereBindExpr entryName)
          alts

      pure bnd{Ghc.fun_matches = mg{ Ghc.mg_alts = newAlts }}
modifyBinding _ _ bnd = pure bnd

mkWhereBindName :: Ghc.TcM Ghc.Name
mkWhereBindName = do
  uniq <- Ghc.getUniqueM
  pure $ Ghc.mkSystemVarName uniq "new_debug_ip"

mkWhereBinding :: Ghc.Name -> Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsBind Ghc.GhcRn
mkWhereBinding whereBindName whereBindExpr =
  Ghc.noLocA' Ghc.FunBind'
    { Ghc.fun_ext' = mempty
    , Ghc.fun_id' = Ghc.noLocA' whereBindName
    , Ghc.fun_matches' =
        Ghc.MG
          { Ghc.mg_ext = Ghc.NoExtField
          , Ghc.mg_alts = Ghc.noLocA'
            [Ghc.noLocA' Ghc.Match
              { Ghc.m_ext = Ghc.emptyEpAnn
              , Ghc.m_ctxt = Ghc.FunRhs
                  { Ghc.mc_fun = Ghc.noLocA' whereBindName
                  , Ghc.mc_fixity = Ghc.Prefix
                  , Ghc.mc_strictness = Ghc.SrcStrict
                  }
              , Ghc.m_pats = []
              , Ghc.m_grhss = Ghc.GRHSs
                  { Ghc.grhssExt = Ghc.emptyComments'
                  , Ghc.grhssGRHSs =
                    [ Ghc.noLoc $ Ghc.GRHS
                        Ghc.emptyEpAnn
                        []
                        whereBindExpr
                    ]
                  , Ghc.grhssLocalBinds = Ghc.noLoc' $
                      Ghc.EmptyLocalBinds Ghc.NoExtField
                  }
              }
            ]
          , Ghc.mg_origin = Ghc.Generated
          }
    }

-- | Add a where bind for the new value of the IP, then add let bindings to the
-- front of each GRHS to set the new value of the IP in that scope.
modifyMatch
  :: Propagation
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn))
modifyMatch prop whereBindExpr entryName match = do
  whereBindName <- lift mkWhereBindName

  visitedNames <- get

  -- only update the where bindings that don't have Debug
  -- predicates, those that do will be addressed via recursion.
  -- It is also necesarry to descend into potential recursive wheres
  -- but the recursion needs to stop if a known name is found.
  let stopCondition :: Ghc.HsBind Ghc.GhcRn -> Bool
      stopCondition Ghc.FunBind{ Ghc.fun_id = Ghc.L _ funName }
        = S.member funName visitedNames
      stopCondition _ = False

      -- recurse the entire match to add let bindings to all where clauses,
      -- including those belonging to let-bound terms at any nesting depth.
      -- Bindings must be added to let statements in do-blocks as well.
      match'@Ghc.Match
        { Ghc.m_grhss =
            grhs@Ghc.GRHSs
              { Ghc.grhssLocalBinds =
#if MIN_VERSION_ghc(9,2,0)
                  whereBinds
#else
                  Ghc.L whereLoc whereBinds
#endif
              , Ghc.grhssGRHSs = grhsList
              }
        } = Syb.everywhereBut
              (Syb.mkQ False stopCondition)
              (Syb.mkT $ updateDebugIpInFunBind whereBindName)
              match

      ipValWhereBind = mkWhereBinding whereBindName whereBindExpr

      wrappedBind = (Ghc.NonRecursive, Ghc.unitBag ipValWhereBind)

      -- add the generated bind to the function's where clause
      whereBinds' =
        case whereBinds of
          Ghc.EmptyLocalBinds _ ->
            Ghc.HsValBinds Ghc.emptyEpAnn
              (Ghc.XValBindsLR (Ghc.NValBinds [wrappedBind] []))

          Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) ->
             Ghc.HsValBinds x
               (Ghc.XValBindsLR
                 (Ghc.NValBinds (wrappedBind : binds) sigs
                 )
               )

          _ -> whereBinds

  pure match'{ Ghc.m_grhss = grhs
                 { Ghc.grhssLocalBinds =
#if MIN_VERSION_ghc(9,2,0)
                     whereBinds'
#else
                     Ghc.L whereLoc whereBinds'
#endif
                 , Ghc.grhssGRHSs =
                     fmap ( updateDebugIPInGRHS whereBindName
                     -- Don't emit entry event if propagation is Mute
                          . if prop == Mute
                               then id
                               else emitEntryEvent entryName
                          )
                       <$> grhsList
                 }
             }

-- | Targets function bindings that are known to not have a debug constraint
-- and then updates the definitions of those functions to add the special let
-- statement referencing the where binding.
updateDebugIpInFunBind
  :: Ghc.Name
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
updateDebugIpInFunBind whereVarName
    b@Ghc.FunBind{ Ghc.fun_matches = m@Ghc.MG{ Ghc.mg_alts = alts } }
  = b { Ghc.fun_matches =
        m { Ghc.mg_alts = (fmap . fmap . fmap) updateMatch alts }
      }
  where
    updateMatch mtch@Ghc.Match{Ghc.m_grhss = g@Ghc.GRHSs{Ghc.grhssGRHSs = grhss}}
      = mtch{Ghc.m_grhss =
               g{Ghc.grhssGRHSs = fmap (updateDebugIPInGRHS whereVarName) <$> grhss }
            }
#if MIN_VERSION_ghc(9,0,0)
#else
    updateMatch x = x
#endif
updateDebugIpInFunBind _ b = b

-- TODO have some warning when optimizations are turned on.

-- | Produce the contents of the where binding that contains the new debug IP
-- value, generated by creating a new ID and pairing it with the old one.
-- The ID is randomly generated. Could instead have a global ID sequence but
-- the random ID has the advantage that a program can be run multiple times
-- using the same log file and the traces won't conflict.
mkNewIpExpr
  :: Either FunName UserKey
  -> Propagation
  -> Ghc.TcM (Ghc.LHsExpr Ghc.GhcRn)
mkNewIpExpr newKey newProp = do
  Right exprPs
    <- fmap (Ghc.convertToHsExpr Ghc.Generated Ghc.noSrcSpan)
     . liftIO
     -- This sometimes gets floated out when optimizations are on. Until this
     -- can be fixed, should compile with -O0 when using the plugin.
     $ TH.runQ [| noinline $ Just $ mkNewDebugContext newKey newProp ?_debug_ip |]

  (exprRn, _) <- Ghc.rnLExpr exprPs

  pure exprRn

-- | Build a new debug context from the previous state
mkNewDebugContext
  :: Either FunName UserKey
  -> Propagation
  -> Maybe DebugContext
  -> DebugContext
mkNewDebugContext newKey newProp mPrevCtx =
  case (mPrevCtx, newKey) of
    -- If override key matches with previous tag, keep the id
    (Just prevCtx, Right userKey)
      | debugKey (currentTag prevCtx) == Right userKey
      -> prevCtx
           { propagation = getNextProp (Just $ propagation prevCtx) }
    _ -> unsafePerformIO $ do
      newId <- Rand.randomIO :: IO Word
      let newTag = DT
            { invocationId = newId
            , debugKey = newKey
            }
      pure
        DC { previousTag = currentTag <$> mPrevCtx
           , currentTag = newTag
           , propagation = getNextProp (propagation <$> mPrevCtx)
           }
  where
    getNextProp Nothing = newProp
    getNextProp (Just prev) =
      case (prev, newProp) of
        (Mute, _) -> Mute
        (_, Mute) -> Mute
        (Deep, _) -> Deep
        _    -> newProp

-- | Wraps an expression with the 'entry' function. '$' is used to apply it
-- because it has same special impredicative type properties in ghc 9.2+.
emitEntryEvent
  :: Ghc.Name
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
emitEntryEvent emitEntryName (Ghc.GRHS x guards body) =
  Ghc.GRHS x guards . Ghc.noLocA' $
    Ghc.HsApp Ghc.emptyEpAnn
      (Ghc.noLocA' $
        Ghc.HsApp Ghc.emptyEpAnn
          (Ghc.noLocA' . Ghc.HsVar Ghc.NoExtField $ Ghc.noLocA' Ghc.dollarName)
          (Ghc.noLocA' . Ghc.HsVar Ghc.NoExtField $ Ghc.noLocA' emitEntryName)
      )
      body
#if MIN_VERSION_ghc(9,0,0)
#else
emitEntryEvent _ x = x
#endif

-- | Given the name of the variable to assign to the debug IP, create a let
-- expression as a guard statement that updates the IP in that scope.
updateDebugIPInGRHS
  :: Ghc.Name
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
updateDebugIPInGRHS whereBindName (Ghc.GRHS x guards body)
  = Ghc.GRHS x (ipUpdateGuard : guards) body
  where
    ipUpdateGuard =
      Ghc.noLocA' $
        Ghc.LetStmt Ghc.emptyEpAnn $
          Ghc.noLoc' $
            Ghc.HsIPBinds Ghc.emptyEpAnn $
              Ghc.IPBinds Ghc.NoExtField
                [ Ghc.noLocA' $ Ghc.IPBind
                    Ghc.emptyEpAnn
                    (Left . Ghc.noLoc $ Ghc.HsIPName "_debug_ip")
                    (Ghc.noLocA' . Ghc.HsVar Ghc.NoExtField
                      $ Ghc.noLocA' whereBindName
                    )
                ]
#if MIN_VERSION_ghc(9,0,0)
#else
updateDebugIPInGRHS _ x = x
#endif

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = pure ()
    , Ghc.tcPluginStop = \_ -> pure ()
    , Ghc.tcPluginSolve = const tcPluginSolver
    }

-- ppr :: Ghc.Outputable a => a -> String
-- ppr = Ghc.showSDocUnsafe . Ghc.ppr

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

-- TODO can the solver be replaced by a global IP instance?

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
