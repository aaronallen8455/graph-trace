{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Debug
  ( plugin
  , trace
  , entry
  , module DT
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar
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
import           System.IO
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

import           Debug.Internal.Types
import qualified Debug.Internal.Types as DT

import qualified Debug.Trace as D

logFilePath :: FilePath
logFilePath = "debug_log.txt"

fileLock :: MVar Handle
fileLock = unsafePerformIO $ do
  h <- openFile logFilePath AppendMode
  hSetBuffering h NoBuffering
  newMVar h
{-# NOINLINE fileLock  #-}

trace :: (?_debug_ip :: Maybe DebugIPTy) => String -> a -> a
trace msg x =
  case ?_debug_ip of
    Nothing -> x
    Just ip -> unsafePerformIO $ do
      withMVar fileLock $ \h -> do
        let ev = TraceEvent (snd ip) msg
        hPutStrLn h $ eventToLogStr ev
      pure x
{-# NOINLINE trace  #-}

entry :: (?_debug_ip :: Maybe DebugIPTy) => a -> a
entry x =
  case ?_debug_ip of
    Nothing -> x
    Just ip -> unsafePerformIO $ do
      withMVar fileLock $ \h -> do
        let ev = EntryEvent (snd ip) (fst ip)
        hPutStrLn h $ eventToLogStr ev
      pure x
{-# NOINLINE entry  #-}

-- TODO include an option that makes all functions that have signatures get
-- automatically instrumented.
-- If the option is engaged, will modify all fun bind signatures in the AST to
-- include the Debug constraint. Not easy to discriminate on fun binds so will
-- just target val binds and modify the list of singatures therein.
-- Then the guard on membership in the map will be removed. But that's not enough
-- because it would include bindings that don't have signatures. Maybe we just
-- need to collect all the signatures anyways. So then the only change would be
-- to add a pass that adds the constraint to all signatures. That is simple
-- enough but performance may start becoming rather poor.

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

  Ghc.Found _ debugModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug") Nothing

  debugPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "Debug")
  debugKeyPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugKey")
  entryName <- Ghc.lookupOrig debugModule (Ghc.mkVarOcc "entry")

  -- If the "debug-all" option is passed, add the Debug predicate to all
  -- function signatures.
  let hsGroup'@Ghc.HsGroup
        { Ghc.hs_valds = Ghc.XValBindsLR (Ghc.NValBinds binds sigs)
        , Ghc.hs_tyclds = tyClGroups
        } = if "debug-all" `elem` cmdLineOptions
            then Syb.mkT (addConstraintToSig debugPredName debugKeyPredName)
                   `Syb.everywhere` hsGroup
            else hsGroup

  -- find all uses of debug predicates in type signatures
  let nameMap =
        Syb.everything M.union
          (Syb.mkQ mempty $ sigUsesDebugPred debugPredName debugKeyPredName)
          hsGroup'

  -- Find the functions corresponding to those signatures and modify their definition.
  binds' <-
    Syb.mkM (modifyBinding nameMap entryName)
      `Syb.everywhereM` binds

  tyClGroups' <-
    traverse
      (modifyTyClGroup debugPredName debugKeyPredName entryName)
      tyClGroups

  pure ( tcGblEnv
       , hsGroup' { Ghc.hs_valds = Ghc.XValBindsLR $ Ghc.NValBinds binds' sigs
                  , Ghc.hs_tyclds = tyClGroups'
                  }
       )
renamedResultAction _ tcGblEnv group = pure (tcGblEnv, group)

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
      pred = Ghc.noLoc $ Ghc.HsTyVar Ghc.NoExtField Ghc.NotPromoted (Ghc.noLoc debugPred)
      go ty =
        case ty of
          Ghc.HsForAllTy x tele body -> Ghc.HsForAllTy x tele $ go <$> body
          q@(Ghc.HsQualTy x ctx body)
            | _ : _ <-
                mapMaybe (checkForDebugPred debugPred debugKeyPred)
                  (Ghc.unLoc $ map Ghc.unLoc <$> ctx)
            -> q
            | otherwise -> Ghc.HsQualTy x (fmap (pred :) ctx) body
          _ -> Ghc.HsQualTy Ghc.NoExtField (Ghc.noLoc [pred]) (Ghc.noLoc ty)

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
sigUsesDebugPred debugPredName debugKeyPredName
  sig@(Ghc.ClassOpSig _ _ lNames (Ghc.HsIB _
    (Ghc.L _ (Ghc.HsQualTy _ (Ghc.L _ ctx) _)))) =
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
  -> Ghc.Name
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> Ghc.TcM (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
modifyBinding nameMap emitEntryName
  bnd@(Ghc.FunBind _ (Ghc.L _ name) mg@(Ghc.MG _ alts _) _)
    | Just mUserKey <- M.lookup name nameMap
    = do
      let key = case mUserKey of
                  Nothing -> Left $ Ghc.getOccString name
                  Just k -> Right $ Ghc.unpackFS k

      whereBindExpr <- mkNewIpExpr key

      newAlts <-
        (traverse . traverse . traverse)
          (modifyMatch nameMap whereBindExpr emitEntryName)
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

-- TODO as an optimization, it doesn't seem necessary to gather all the Names
-- from the sigs all at once, can probably look at them when examining the
-- where bound matches.
-- The problem is that syb doesn't offer a way to include context when doing
-- a modification and rolling it into a monadic context won't work because it
-- is a bottom up transform. Would it be possible to use the 'somewhere' scheme
-- and have it make recursive calls that include the extra context? What exactly
-- does somewhere do? Doesn't seem like it'd work.

-- | Add a let binding setting the new value of the IP to each where bound
-- function that does not exist in the map.
addLetToWhereBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
addLetToWhereBinds nameMap whereBindName
  m@Ghc.Match
    { Ghc.m_grhss =
        grhs@Ghc.GRHSs
          { Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds }
    } =
  let newWhereBinds =
        case whereBinds of
          Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) ->
            let binds' =
                  fmap (updateDebugIPInBinds nameMap whereBindName)
                   <$> binds
             in Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds' sigs))
          _ -> whereBinds
   in m{ Ghc.m_grhss =
           grhs{ Ghc.grhssLocalBinds = Ghc.L whereLoc newWhereBinds }
       }

-- | Add a where bind for the new value of the IP, then add let bindings to the
-- front of each GRHS to set the new value of the IP in that scope.
modifyMatch
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.Name
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.TcM (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn))
modifyMatch nameMap whereBindExpr emitEntryName match = do
  whereBindName <- mkWhereBindName

  -- only update the where bindings that don't have Debug
  -- predicates, those that do will be addressed via recursion.
  -- It is also necesarry to descend into potential recursive wheres
  -- but the recursion needs to stop if a known name is found.
  let stopCondition :: Ghc.HsBind Ghc.GhcRn -> Bool
      stopCondition b@Ghc.FunBind{ Ghc.fun_id = Ghc.L _ funName }
        = M.member funName nameMap

      -- recurse entire the entire match to add let bindings to all where
      -- clauses, including those belonging to let-bound terms at any
      -- nesting depth
      match'@Ghc.Match
        { Ghc.m_grhss =
            grhs@Ghc.GRHSs
              { Ghc.grhssLocalBinds = Ghc.L whereLoc whereBinds
              , Ghc.grhssGRHSs = grhsList
              }
        } = Syb.everywhereBut
              (Syb.mkQ False stopCondition)
              (Syb.mkT $ addLetToWhereBinds nameMap whereBindName)
              match

      ipValWhereBind = mkWhereBinding whereBindName whereBindExpr

      wrappedBind = (Ghc.NonRecursive, Ghc.unitBag ipValWhereBind)

      -- add the generated bind to the function's where clause
      whereBinds' =
        case whereBinds of
          Ghc.EmptyLocalBinds x ->
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

-- | Looks for function bindings that are known to have a debug constraint and
-- then updates the definitions of those functions to add the special let
-- statement referencing the where binding.
updateDebugIPInBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name
  -> Ghc.LHsBinds Ghc.GhcRn
  -> Ghc.LHsBinds Ghc.GhcRn
updateDebugIPInBinds nameMap whereVarName binds
  = fmap updateBind <$> binds
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

-- | For a group containing class instances and declarations, find method
-- signatures that contain debug constraints, then modify the instance definitions
-- of those functions to add instrumentation.
-- TODO what about where bindings within method definitions (done)? Is this a more general
-- problem where only top level functions with Debug constraints are recursively checked
-- for where bindings that have debug constraints? Yes, where bound functions
-- inside of functions that are not marked for debug will not be treated. Is this
-- really a problem though?
modifyTyClGroup
  :: Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.TyClGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TyClGroup Ghc.GhcRn)
modifyTyClGroup debugName debugKeyName entryName
  tyClGroup@Ghc.TyClGroup{ Ghc.group_instds = instances
                         , Ghc.group_tyclds = tyCls } = do
    let modifyInstance c@Ghc.ClsInstD{ Ghc.cid_inst = inst } = do
          inst' <-
            modifyClsInstDecl tyClNameMap debugName debugKeyName entryName inst
          pure c { Ghc.cid_inst = inst' }
    instances' <- (traverse . traverse) modifyInstance instances

    tyCls' <-
      (traverse . traverse)
        (modifyDefaultTyClImpl tyClNameMap debugName debugKeyName entryName)
        tyCls

    pure tyClGroup { Ghc.group_instds = instances', Ghc.group_tyclds = tyCls' }
  where
    maybeClassDecl c@Ghc.ClassDecl{} = Just c
    maybeClassDecl _ = Nothing
    tyClNameMap =
      foldMap
          ( foldMap (sigUsesDebugPred debugName debugKeyName . Ghc.unLoc)
          . Ghc.tcdSigs
          )
      $ mapMaybe (maybeClassDecl . Ghc.unLoc) tyCls

-- TODO Use a context Reader to pass around names and map

-- | Instrument the default implementations in a class decl
modifyDefaultTyClImpl
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.TyClDecl Ghc.GhcRn
  -> Ghc.TcM (Ghc.TyClDecl Ghc.GhcRn)
modifyDefaultTyClImpl nameMap debugName debugKeyName entryName
    cd@Ghc.ClassDecl { Ghc.tcdMeths = meths } = do
  let innerBindNames =
        Syb.everything M.union
          (Syb.mkQ mempty $ sigUsesDebugPred debugName debugKeyName)
          meths
      nameMap' = innerBindNames <> nameMap

  newMeths <-
    Syb.mkM (modifyBinding nameMap' entryName)
      `Syb.everywhereM` meths

  pure cd { Ghc.tcdMeths = newMeths }
modifyDefaultTyClImpl _ _ _ _ x = pure x

-- | Modify bindings for a type class instance declaration.
modifyClsInstDecl
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.ClsInstDecl Ghc.GhcRn
  -> Ghc.TcM (Ghc.ClsInstDecl Ghc.GhcRn)
modifyClsInstDecl tyClNameMap debugName debugKeyName entryName
    inst@Ghc.ClsInstDecl{ Ghc.cid_binds = binds, Ghc.cid_sigs = sigs }
      = do

  -- This is will collect names from inner where bound functions as well as
  -- instance signatures which might want to override the signature from the
  -- class method definition.
  let getSigName (Ghc.L _ sig)
        = sigUsesDebugPred debugName debugKeyName sig
      allSigNames = foldMap getSigName sigs

      innerBindNames =
        Syb.everything M.union
          (Syb.mkQ mempty $ sigUsesDebugPred debugName debugKeyName)
          binds

      -- Instrumenting class methods only works if the method definition AND
      -- the instance signature have the pred.
      nameMap' = M.unions [innerBindNames, allSigNames, tyClNameMap]

  newBinds <-
    Syb.mkM (modifyBinding nameMap' entryName)
      `Syb.everywhereM` binds

  pure inst { Ghc.cid_binds = newBinds }

-- | Produce the contents of the where binding that contains the new debug IP
-- value, generated by creating a new ID and pairing it with the old one.
mkNewIpExpr :: Either FunName UserKey -> Ghc.TcM (Ghc.LHsExpr Ghc.GhcRn)
mkNewIpExpr newKey = do
  Right exprPs
    <- fmap (Ghc.convertToHsExpr Ghc.Generated Ghc.noSrcSpan)
     . liftIO
     -- Writing it this way prevents GHC from floating this out with -O2.
     -- The call to noinline doesn't seem to contribute, but who knows.
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
