{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug
  ( plugin
  , module DT
  , module Trace
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import qualified Data.Generics as Syb
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           GHC.Exts (noinline)
import qualified Language.Haskell.TH as TH
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as Rand

import qualified GHC.Builtin.Names as Ghc
import qualified GHC.Builtin.Types as Ghc
import qualified GHC.Core.Class as Ghc
import qualified GHC.Core.Make as Ghc
import qualified GHC.Core.Type as Ghc
import qualified GHC.Data.Bag as Ghc
import qualified GHC.Data.FastString as Ghc
import qualified GHC.Driver.Finder as Ghc
import qualified GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import qualified GHC.Hs.Binds as Ghc
import qualified GHC.Hs.Decls as Ghc
import qualified GHC.Hs.Expr as Ghc
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Hs.Type as Ghc
import qualified GHC.Iface.Env as Ghc
import qualified GHC.Rename.Expr as Ghc
import qualified GHC.Tc.Types as Ghc
import qualified GHC.Tc.Types.Constraint as Ghc
import qualified GHC.Tc.Types.Evidence as Ghc
import qualified GHC.Tc.Types.Origin as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.ThToHs as Ghc
import qualified GHC.Types.Basic as Ghc
import qualified GHC.Types.Name as Ghc hiding (varName)
import qualified GHC.Types.SrcLoc as Ghc
import qualified GHC.Types.Unique.Supply as Ghc
import qualified GHC.Unit.Module.Name as Ghc
import qualified GHC.Utils.Outputable as Ghc

import           Debug.Internal.Types
import qualified Debug.Internal.Types as DT
import           Debug.Internal.Trace as Trace

import qualified Debug.Trace as D

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

renamedResultAction
  :: [Ghc.CommandLineOption]
  -> Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction cmdLineOptions tcGblEnv
    hsGroup@Ghc.HsGroup{Ghc.hs_valds = Ghc.XValBindsLR{}}
    = do
  hscEnv <- Ghc.getTopEnv

  Ghc.Found _ debugTypesModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug.Internal.Types") Nothing

  Ghc.Found _ debugTraceModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug.Internal.Trace") Nothing

  debugPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "Debug")
  debugKeyPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugKey")
  entryName <- Ghc.lookupOrig debugTraceModule (Ghc.mkVarOcc "entry")

  -- If the "debug-all" option is passed, add the Debug predicate to all
  -- function signatures.
  let hsGroup'@Ghc.HsGroup
        { Ghc.hs_valds = valBinds --Ghc.XValBindsLR (Ghc.NValBinds binds sigs)
        , Ghc.hs_tyclds = tyClGroups
        } = if "debug-all" `elem` cmdLineOptions
            then Syb.mkT (addConstraintToSig debugPredName debugKeyPredName)
                   `Syb.everywhere` hsGroup
            else hsGroup

  -- process value bindings
  valBinds' <- (`evalStateT` S.empty) $
      Syb.mkM (modifyValBinds debugPredName debugKeyPredName entryName)
    `Syb.everywhereM`
      valBinds

  -- process type class decls and instances
  -- TODO Only need to traverse with modifyValBinds. Other are not applied deeply
  tyClGroups' <- (`evalStateT` S.empty) $
      Syb.mkM (modifyClsInstDecl debugPredName debugKeyPredName entryName)
    `Syb.extM`
      modifyTyClDecl debugPredName debugKeyPredName entryName
    `Syb.extM`
      modifyValBinds debugPredName debugKeyPredName entryName
    `Syb.everywhereM`
      tyClGroups

  pure ( tcGblEnv
       , hsGroup' { Ghc.hs_valds = valBinds'
                  , Ghc.hs_tyclds = tyClGroups'
                  }
       )

renamedResultAction _ tcGblEnv group = pure (tcGblEnv, group)

-- | Find all function names that have a type signature containing a debug pred.
-- If the DebugKey pred is found, record its assigned string.
collectNames
  :: Ghc.Name
  -> Ghc.Name
  -> [Ghc.LSig Ghc.GhcRn]
  -> M.Map Ghc.Name (Maybe Ghc.FastString)
collectNames debugPred debugKeyPred =
  (foldMap . foldMap)
    (sigUsesDebugPred debugPred debugKeyPred)

-- | Instrument a set of bindings given a Map containing the names of functions
-- that should be modified.
modifyBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name
  -> Ghc.LHsBinds Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.LHsBinds Ghc.GhcRn)
modifyBinds nameMap entryName =
  (traverse . traverse)
    (modifyBinding nameMap entryName)

-- | Instrument value bindings that have a signature with a debug pred.
modifyValBinds
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.Name
  -> Ghc.NHsValBindsLR Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.NHsValBindsLR Ghc.GhcRn)
modifyValBinds debugPred debugKeyPred entryName (Ghc.NValBinds binds sigs) = do
  let nameMap = collectNames debugPred debugKeyPred sigs
  binds' <- (traverse . traverse) (modifyBinds nameMap entryName) binds
  modify' (S.union $ M.keysSet nameMap)
  pure $ Ghc.NValBinds binds' sigs

-- | Instrument default method implementations in a type class declaration if
-- they contain a Debug pred.
modifyTyClDecl
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.Name
  -> Ghc.TyClDecl Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.TyClDecl Ghc.GhcRn)
modifyTyClDecl debugPred debugKeyPred entryName
    cd@Ghc.ClassDecl { Ghc.tcdMeths = meths
                     , Ghc.tcdSigs = sigs
                     } = do
  let nameMap = collectNames debugPred debugKeyPred sigs
  newMeths <- modifyBinds nameMap entryName meths
  pure cd { Ghc.tcdMeths = newMeths }
modifyTyClDecl _ _ _ x = pure x

-- | Instrument the method implementations in an type class instance if it has
-- a signature containing a debug pred.
modifyClsInstDecl
  :: Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.ClsInstDecl Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.ClsInstDecl Ghc.GhcRn)
modifyClsInstDecl debugName debugKeyName entryName
    inst@Ghc.ClsInstDecl{ Ghc.cid_binds = binds, Ghc.cid_sigs = sigs }
      = do
  let nameMap = collectNames debugName debugKeyName sigs
  newBinds <- modifyBinds nameMap entryName binds
  pure inst { Ghc.cid_binds = newBinds }

-- | Matches on type signatures in order to add the constraint to them.
addConstraintToSig
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.Sig Ghc.GhcRn
  -> Ghc.Sig Ghc.GhcRn
addConstraintToSig debugPred debugKeyPred
  (Ghc.TypeSig x1 lNames (Ghc.HsWC x2 sig)) =
    Ghc.TypeSig x1 lNames (Ghc.HsWC x2
      (addConstraintToSigType debugPred debugKeyPred sig))
addConstraintToSig debugPred debugKeyPred
  (Ghc.ClassOpSig x1 b lNames sig) =
    Ghc.ClassOpSig x1 b lNames
      (addConstraintToSigType debugPred debugKeyPred sig)
addConstraintToSig _ _ s = s

-- | Adds the 'Debug' constraint to a signature if it doesn't already have it
-- as the first constraint in the context.
addConstraintToSigType
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.LHsSigType Ghc.GhcRn
  -> Ghc.LHsSigType Ghc.GhcRn
addConstraintToSigType debugPred debugKeyPred sig@Ghc.HsIB{ Ghc.hsib_body = t } =
  sig{ Ghc.hsib_body = fmap go t }
    where
      predTy = Ghc.noLoc $ Ghc.HsTyVar Ghc.NoExtField Ghc.NotPromoted (Ghc.noLoc debugPred)
      go ty =
        case ty of
          Ghc.HsForAllTy x tele body -> Ghc.HsForAllTy x tele $ go <$> body
          q@(Ghc.HsQualTy x ctx body)
            | _ : _ <-
                mapMaybe (checkForDebugPred debugPred debugKeyPred)
                  (Ghc.unLoc $ map Ghc.unLoc <$> ctx)
            -> q
            | otherwise -> Ghc.HsQualTy x (fmap (predTy :) ctx) body
          _ -> Ghc.HsQualTy Ghc.NoExtField (Ghc.noLoc [predTy]) (Ghc.noLoc ty)

-- | If a sig contains the Debug constraint, get the name of the corresponding
-- binding.
--
-- Are there ever more than one name in the TypeSig? yes:
-- one, two :: Debug x => ...
sigUsesDebugPred
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.Sig Ghc.GhcRn
  -> M.Map Ghc.Name (Maybe Ghc.FastString)
sigUsesDebugPred debugPredName debugKeyPredName sig =
  case sig of
    (Ghc.TypeSig _ lNames (Ghc.HsWC _ (Ghc.HsIB _
      (Ghc.L _ (Ghc.HsQualTy _ (Ghc.L _ ctx) _)))))
        -> collect lNames ctx
    (Ghc.ClassOpSig _ _ lNames (Ghc.HsIB _
      (Ghc.L _ (Ghc.HsQualTy _ (Ghc.L _ ctx) _))))
        -> collect lNames ctx
    _ -> mempty
  where
    collect lNames ctx =
      let mKey = listToMaybe
           $ mapMaybe (checkForDebugPred debugPredName debugKeyPredName)
                      (Ghc.unLoc <$> ctx)
       in case mKey of
            Nothing -> mempty
            Just key -> M.fromList $ zip (Ghc.unLoc <$> lNames) (repeat key)

checkForDebugPred
  :: Ghc.Name
  -> Ghc.Name
  -> Ghc.HsType Ghc.GhcRn
  -> Maybe (Maybe Ghc.FastString)
checkForDebugPred debugPredName _
    (Ghc.HsTyVar _ _ (Ghc.L _ name))
  | name == debugPredName = Just Nothing
checkForDebugPred _ debugKeyPredName
    (Ghc.HsAppTy _ (Ghc.L _ (Ghc.HsTyVar _ _ (Ghc.L _ name))) (Ghc.L _ (Ghc.HsTyLit _ (Ghc.HsStrTy _ key))))
  | name == debugKeyPredName = Just (Just key)
checkForDebugPred n nk (Ghc.HsForAllTy _ _ (Ghc.L _ ty)) = checkForDebugPred n nk ty
checkForDebugPred n nk (Ghc.HsParTy _ (Ghc.L _ ty)) = checkForDebugPred n nk ty
checkForDebugPred _ _ _ = Nothing
-- need a case for nested QualTy?

-- | Instrument a binding if its name is in the Map.
modifyBinding
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
modifyBinding nameMap emitEntryName
  bnd@(Ghc.FunBind _ (Ghc.L _ name) mg@(Ghc.MG _ alts _) _)
    | Just mUserKey <- M.lookup name nameMap
    = do
      let key = case mUserKey of
                  Nothing -> Left $ Ghc.getOccString name
                  Just k -> Right $ Ghc.unpackFS k

      whereBindExpr <- lift $ mkNewIpExpr key

      newAlts <-
        (traverse . traverse . traverse)
          (modifyMatch whereBindExpr emitEntryName)
          alts

      pure bnd{Ghc.fun_matches = mg{ Ghc.mg_alts = newAlts }}
modifyBinding _ _ bnd = pure bnd

mkWhereBindName :: Ghc.TcM Ghc.Name
mkWhereBindName = do
  uniq <- Ghc.getUniqueM
  pure $ Ghc.mkSystemVarName uniq "new_debug_ip"

mkWhereBinding :: Ghc.Name -> Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsBind Ghc.GhcRn
mkWhereBinding whereBindName whereBindExpr =
  Ghc.noLoc Ghc.FunBind
    { Ghc.fun_ext = mempty
    , Ghc.fun_id = Ghc.noLoc whereBindName
    , Ghc.fun_matches =
        Ghc.MG
          { Ghc.mg_ext = Ghc.NoExtField
          , Ghc.mg_alts = Ghc.noLoc
            [Ghc.noLoc Ghc.Match
              { Ghc.m_ext = Ghc.NoExtField
              , Ghc.m_ctxt = Ghc.FunRhs
                  { Ghc.mc_fun = Ghc.noLoc whereBindName
                  , Ghc.mc_fixity = Ghc.Prefix
                  , Ghc.mc_strictness = Ghc.SrcStrict
                  }
              , Ghc.m_pats = []
              , Ghc.m_grhss = Ghc.GRHSs
                  { Ghc.grhssExt = Ghc.NoExtField
                  , Ghc.grhssGRHSs =
                    [ Ghc.noLoc $ Ghc.GRHS
                        Ghc.NoExtField
                        []
                        whereBindExpr
                    ]
                  , Ghc.grhssLocalBinds = Ghc.noLoc $
                      Ghc.EmptyLocalBinds Ghc.NoExtField
                  }
              }
            ]
          , Ghc.mg_origin = Ghc.Generated
          }
    , Ghc.fun_tick = []
    }

-- | Add a where bind for the new value of the IP, then add let bindings to the
-- front of each GRHS to set the new value of the IP in that scope.
modifyMatch
  :: Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn))
modifyMatch whereBindExpr emitEntryName match = do
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
              { Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds
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
            Ghc.HsValBinds Ghc.NoExtField
              (Ghc.XValBindsLR (Ghc.NValBinds [wrappedBind] []))

          Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) ->
             Ghc.HsValBinds x
               (Ghc.XValBindsLR
                 (Ghc.NValBinds (wrappedBind : binds) sigs
                 )
               )

          _ -> whereBinds

  pure match'{ Ghc.m_grhss = grhs
                 { Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds'
                 , Ghc.grhssGRHSs =
                     fmap ( updateDebugIPInGRHS whereBindName
                          . emitEntryEvent emitEntryName
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
updateDebugIpInFunBind _ b = b

-- TODO have some warning when optimizations are turned on.

-- | Produce the contents of the where binding that contains the new debug IP
-- value, generated by creating a new ID and pairing it with the old one.
mkNewIpExpr :: Either FunName UserKey -> Ghc.TcM (Ghc.LHsExpr Ghc.GhcRn)
mkNewIpExpr newKey = do
  Right exprPs
    <- fmap (Ghc.convertToHsExpr Ghc.Generated Ghc.noSrcSpan)
     . liftIO
     -- This sometimes gets floated out when optimizations are on. Until this
     -- can be fixed, should compile with -O0 when using the plugin.
     $ TH.runQ [| noinline $
                  let mPrevTag = fmap snd ?_debug_ip
                   in case (mPrevTag, newKey) of
                        -- If override key matches with previous tag, keep the id
                        (Just prevTag, Right userKey)
                          | debugKey prevTag == Right userKey
                          -> ?_debug_ip
                        _ -> unsafePerformIO $ do
                          newId <- Rand.randomIO :: IO Word
                          let newTag = DT
                                { invocationId = newId
                                , debugKey = newKey
                                }
                          pure $ Just (mPrevTag, newTag)
               |]

  (exprRn, _) <- Ghc.rnLExpr exprPs

  pure exprRn

emitEntryEvent
  :: Ghc.Name
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
emitEntryEvent emitEntryName (Ghc.GRHS x guards body) =
  Ghc.GRHS x guards . Ghc.noLoc $
    Ghc.HsApp Ghc.NoExtField
      (Ghc.noLoc . Ghc.HsVar Ghc.NoExtField $ Ghc.noLoc emitEntryName)
      body

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
      Ghc.noLoc $
        Ghc.LetStmt Ghc.NoExtField $
          Ghc.noLoc $
            Ghc.HsIPBinds Ghc.NoExtField $
              Ghc.IPBinds Ghc.NoExtField
                [ Ghc.noLoc $ Ghc.IPBind
                    Ghc.NoExtField
                    (Left . Ghc.noLoc $ Ghc.HsIPName "_debug_ip")
                    (Ghc.noLoc . Ghc.HsVar Ghc.NoExtField
                      $ Ghc.noLoc whereBindName
                    )
                ]

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = pure ()
    , Ghc.tcPluginStop = \_ -> pure ()
    , Ghc.tcPluginSolve = const tcPluginSolver
    }

ppr :: Ghc.Outputable a => a -> String
ppr = Ghc.showSDocUnsafe . Ghc.ppr

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
tcPluginSolver [] [] wanted = do
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
