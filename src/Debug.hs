{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Debug where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable
import           Data.Functor.Const
import qualified Data.Generics as Syb
import           Data.Traversable
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           GHC.Exts (noinline)
import           GHC.TypeLits (Symbol)
import qualified Language.Haskell.TH as TH
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as Rand

import qualified GHC.Builtin.Names as Ghc
import qualified GHC.Builtin.Types as Ghc
import qualified GHC.Core as Ghc
import qualified GHC.Core.Class as Ghc
import qualified GHC.Core.Make as Ghc
import qualified GHC.Core.Type as Ghc
import qualified GHC.Core.Utils as Ghc
import qualified GHC.Data.Bag as Ghc
import qualified GHC.Data.FastString as Ghc
import qualified GHC.Driver.Finder as Ghc
import qualified GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import qualified GHC.Driver.Types as Ghc
import qualified GHC.Hs.Binds as Ghc
import qualified GHC.Hs.Decls as Ghc
import qualified GHC.Hs.Expr as Ghc
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Hs.Type as Ghc
import qualified GHC.Iface.Env as Ghc
import qualified GHC.Rename.Expr as Ghc
import qualified GHC.Tc.Plugin as Ghc hiding (lookupOrig, findImportedModule, getTopEnv)
import qualified GHC.Tc.Types as Ghc
import qualified GHC.Tc.Types.Constraint as Ghc
import qualified GHC.Tc.Types.Evidence as Ghc
import qualified GHC.Tc.Types.Origin as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.ThToHs as Ghc
import qualified GHC.Types.Basic as Ghc
import qualified GHC.Types.Id as Ghc
import qualified GHC.Types.Name as Ghc hiding (varName)
import qualified GHC.Types.Name.Occurrence as Ghc hiding (varName)
import qualified GHC.Types.SrcLoc as Ghc
import qualified GHC.Types.Unique.Supply as Ghc
import qualified GHC.Types.Var as Ghc
import qualified GHC.Unit.Module.Name as Ghc
import qualified GHC.Utils.Outputable as Ghc

type Debug = (?_debug_ip :: Maybe (Maybe String, String)) -- (DebugKey key, ?_debug_ip :: String)
type DebugKey (key :: Symbol) = (?_debug_ip :: Maybe (Maybe String, String)) -- (DebugKey key, ?_debug_ip :: String)

trace :: (?_debug_ip :: Maybe (Maybe String, String)) => IO ()
trace = print (?_debug_ip :: Maybe (Maybe String, String))

-- TODO modify dyn flags to include ImplicitParams?
plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.pluginRecompile = Ghc.purePlugin
    , Ghc.tcPlugin = \_ -> Just tcPlugin
    -- , Ghc.typeCheckResultAction = const typeCheckResultAction
    , Ghc.renamedResultAction = const renamedResultAction
    }

renamedResultAction :: Ghc.TcGblEnv -> Ghc.HsGroup Ghc.GhcRn
                    -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction tcGblEnv
    hsGroup@Ghc.HsGroup
      { Ghc.hs_valds =
          Ghc.XValBindsLR (Ghc.NValBinds binds sigs)
      }
    = do
  hscEnv <- Ghc.getTopEnv

  Ghc.Found _ debugModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug") Nothing

  debugPredName <- Ghc.lookupOrig debugModule (Ghc.mkClsOcc "Debug")
  debugKeyPredName <- Ghc.lookupOrig debugModule (Ghc.mkClsOcc "DebugKey")

  -- find all uses of debug predicates in type signatures
  let nameMap =
        Syb.everything M.union
          (Syb.mkQ mempty $ sigUsesDebugPred debugPredName debugKeyPredName)
          hsGroup

  -- Find the functions corresponding to those signatures and modify their definition.
  binds' <-
    Syb.mkM (modifyBinding nameMap)
      `Syb.everywhereM` binds

  pure (tcGblEnv, hsGroup { Ghc.hs_valds = Ghc.XValBindsLR $ Ghc.NValBinds binds' sigs })
renamedResultAction tcGblEnv group = pure (tcGblEnv, group)

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
sigUsesDebugPred debugPredName debugKeyPredName
  sig@(Ghc.TypeSig _ lNames (Ghc.HsWC _ (Ghc.HsIB _
    (Ghc.L _ (Ghc.HsQualTy _ (Ghc.L _ ctx) _))))) =
      let mKey = listToMaybe
           $ mapMaybe (checkForDebugPred debugPredName debugKeyPredName)
                      (Ghc.unLoc <$> ctx)
       in case mKey of
            Nothing -> mempty
            Just key -> M.fromList $ zip (Ghc.unLoc <$> lNames) (repeat key)
sigUsesDebugPred _ _ sig = mempty

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

modifyBinding
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> Ghc.TcM (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
modifyBinding nameMap
  bnd@(Ghc.FunBind _ (Ghc.L _ name) mg@(Ghc.MG _ alts _) _)
    | Just mUserKey <- M.lookup name nameMap
    = do
      let key = maybe (Ghc.getOccString name) Ghc.unpackFS mUserKey

      whereBindExpr <- mkNewIpExpr key

      newAlts <-
        (traverse . traverse . traverse)
          (modifyMatch nameMap whereBindExpr)
          alts

      pure bnd{Ghc.fun_matches = mg{ Ghc.mg_alts = newAlts }}
modifyBinding _ bnd = pure bnd

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
                 , Ghc.mc_strictness = Ghc.NoSrcStrict
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
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.TcM (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn))
modifyMatch nameMap whereBindExpr
  m@Ghc.Match
    { Ghc.m_grhss =
        grhs@Ghc.GRHSs
          { Ghc.grhssGRHSs = grhss
          , Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds
          }
    } = do
      whereBindName <- mkWhereBindName

      let grhss' = fmap (updateDebugIPInGRHS whereBindName) <$> grhss
          ipValWhereBind = mkWhereBinding whereBindName whereBindExpr

          wrappedBind = (Ghc.NonRecursive, Ghc.unitBag ipValWhereBind)

          whereBinds' =
            case whereBinds of
              Ghc.EmptyLocalBinds x ->
                Ghc.HsValBinds Ghc.NoExtField
                  (Ghc.XValBindsLR (Ghc.NValBinds [wrappedBind] []))

              Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) ->
                -- only update the where bindings that don't have Debug
                -- predicates, those that do will be addressed via recursion.
                -- It is also necesarry to descend into potential recursive wheres
                -- but the recursion needs to stop if a known name is found.
                let stopCondition :: Ghc.HsBind Ghc.GhcRn -> Bool
                    stopCondition b@Ghc.FunBind{ Ghc.fun_id = Ghc.L _ funName }
                      = M.member funName nameMap

                    otherBinds =
                      Syb.everywhereBut
                        (Syb.mkQ False stopCondition)
                        (Syb.mkT $ updateDebugIPInBinds nameMap whereBindName)
                        binds

                 in Ghc.HsValBinds x
                      (Ghc.XValBindsLR
                        (Ghc.NValBinds (wrappedBind : otherBinds) sigs
                        )
                      )

              _ -> whereBinds

      pure m { Ghc.m_grhss = grhs
                 { Ghc.grhssGRHSs = grhss'
                 , Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds'
                 }
             }

updateDebugIPInBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name
  -> (Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn)
  -> (Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn)
updateDebugIPInBinds nameMap whereVarName (rec, binds)
  = (rec, fmap updateBind <$> binds)
    where
      updateBind :: Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
      updateBind b@Ghc.FunBind{ Ghc.fun_matches = m@Ghc.MG{ Ghc.mg_alts = alts }
                              , Ghc.fun_id = Ghc.L _ funName
                              }
        | funName `M.notMember` nameMap
        = b { Ghc.fun_matches =
              m { Ghc.mg_alts = (fmap . fmap . fmap) updateMatch alts }
            }
      updateBind b = b
      updateMatch m@Ghc.Match{Ghc.m_grhss = g@Ghc.GRHSs{Ghc.grhssGRHSs = grhss}}
        = m{Ghc.m_grhss =
              g{Ghc.grhssGRHSs = fmap (updateDebugIPInGRHS whereVarName) <$> grhss }
           }

-- | Produce the contents of the where binding that contains the new debug IP
-- value, generated by creating a new ID and pairing it with the old one.
mkNewIpExpr :: String -> Ghc.TcM (Ghc.LHsExpr Ghc.GhcRn)
mkNewIpExpr key = do
  Right exprPs
    <- fmap (Ghc.convertToHsExpr Ghc.Generated Ghc.noSrcSpan)
     . liftIO
     -- Writing it this way prevents GHC from floating this out with -O2.
     -- The call to noinline doesn't seem to contribute, but who knows.
     $ TH.runQ [| noinline $! unsafePerformIO $ do
                    !newId <- fmap show (Rand.randomIO :: IO Word)
                    case ?_debug_ip of
                      Nothing ->
                        pure $ Just (Nothing, key <> newId)
                      Just (_, !prev) ->
                        pure $ Just (Just prev, key <> newId)
               |]

  (exprRn, _) <- Ghc.rnLExpr exprPs

  pure exprRn

-- Need to modify let bindings as well. And what going recursively into where
-- and let bindings? a where binding can have its own where clause etc.
-- Could do a syb recursion that stops when a function is found that we have
-- a name in the map for.
-- Let bindings don't matter
-- What about where clauses of let bindings? Will need to recurse into GRH
-- as well as local binds? Yes

updateDebugIPInGRHS
  :: Ghc.Name
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
updateDebugIPInGRHS whereBindName (Ghc.GRHS x guards body)
  = Ghc.GRHS x guards (updateDebugIPInExpr whereBindName body)

-- | Given the name of the variable to assign to the debug IP, create a let
-- expression that updates the IP in that scope.
updateDebugIPInExpr
  :: Ghc.Name
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.LHsExpr Ghc.GhcRn
updateDebugIPInExpr whereBindName
  = Ghc.noLoc
  . Ghc.HsLet Ghc.NoExtField
      ( Ghc.noLoc $ Ghc.HsIPBinds
          Ghc.NoExtField
          ( Ghc.IPBinds Ghc.NoExtField
              [ Ghc.noLoc $ Ghc.IPBind
                  Ghc.NoExtField
                  (Left . Ghc.noLoc $ Ghc.HsIPName "_debug_ip")
                  (Ghc.noLoc $ Ghc.HsVar Ghc.NoExtField
                    (Ghc.noLoc whereBindName)
                  )
              ]
          )
      )

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = pure () -- Ghc.tcPluginIO $ newIORef False
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
  | otherwise = False

tcPluginSolver :: Ghc.TcPluginSolver
tcPluginSolver [] [] wanted = do
  -- Ghc.tcPluginIO . putStrLn $ ppr (wanted, given, derived)
  case filter isDebuggerIpCt wanted of

    [w]
      | Ghc.IPOccOrigin _ <- Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
      -> do
        --Ghc.tcPluginIO . putStrLn . ppr $ Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
        pure $ Ghc.TcPluginOk [] []
      | otherwise
      -> do
           --Ghc.tcPluginIO . putStrLn . ppr $ Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
           let tupFstTy = Ghc.mkTyConApp Ghc.maybeTyCon [Ghc.stringTy]
               tupSndTy = Ghc.stringTy
               tupTy = Ghc.mkTyConApp Ghc.maybeTyCon
                       [Ghc.mkTupleTy Ghc.Boxed [tupFstTy, tupSndTy]]
               expr = Ghc.mkNothingExpr tupTy
           pure $ Ghc.TcPluginOk [(Ghc.EvExpr expr, w)] []
    _ -> pure $ Ghc.TcPluginOk [] []
tcPluginSolver _ _ _ = pure $ Ghc.TcPluginOk [] []
