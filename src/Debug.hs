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

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.pluginRecompile = Ghc.purePlugin
    , Ghc.tcPlugin = \_ -> Just tcPlugin
    , Ghc.renamedResultAction = const renamedResultAction
    }

-- Now that the IP is being updated correctly, will need to start thinking
-- about how best to construct and emit the events (for both tracing and on
-- function entry). What sort of UI should be used to construct and display
-- the graph? graphviz? whatever ghcviz uses?
-- Looks like graphviz is a good starting point. Can use the "record" shape
-- with segmented labels to show the history of a particular function call
-- with edges pointing out to child function calls.
-- Should use HTML-like labels instead
-- Only the entry events needs to emit the predecessor ID and header info,
-- trace messages just emit the function's id along with the message.
-- How should emission occur? Would be nice to write to a file but will
-- interleaving output work with that? does the file lock when one thread is
-- writing to it so that another thread trying to write will have to wait?
-- appendFile seems like the best bet.
-- This will not work with concurrency. Instead will need to have an MVar to
-- restrict access to the file. But how will this global state be introduced?
-- It would need to be shared across all modules. That does not seem possible.
-- Will need to have multiple files - at a minimum one per module and then have
-- a shared global MVar declaration added to that module.
-- Having one file per function would be way too many files, so it has to be on
-- a per module basis.
-- So the plan is:
-- Add a toplevel binding to create an MVar with unsafePerformIO. It needs to
-- have a noInline pragma. Is it possible to add that as part of the parsed
-- AST? Probably need to do it as part of a parse result plugin rather than
-- a rename result action. The pragmas are found in the Sig type
-- For starters, would be easiest to have a top level MVar in the plugin module
-- that controls access to the file handle and only have a single file. This is
-- not ideal though because there could be high contention for this MVar.
-- Could potentially split up along module lines using something like the technique
-- described above although then it becomes essential to record the time stamp
-- of all events so that the order of occurrence can be reconstructed.
-- What should written to the file for each event?
-- for the entry event:
-- entry|module|functionName|invocationId|parentName|parentId(optional)\n
-- trace|functionName|invocationId|message\n
-- TODO what about class methods? Will need to fix this
-- TODO what about traces places in guards? Will probably need to put the let
-- statement within each guard rather than in the body (but only if there are
-- existing guards?)
-- TODO will need to have NOINLINE pragmas on all the trace definitions

renamedResultAction :: Ghc.TcGblEnv -> Ghc.HsGroup Ghc.GhcRn
                    -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction tcGblEnv
    hsGroup@Ghc.HsGroup
      { Ghc.hs_valds =
          Ghc.XValBindsLR (Ghc.NValBinds binds sigs)
      , Ghc.hs_tyclds = tyClGroups
      }
    = do
  hscEnv <- Ghc.getTopEnv

  Ghc.Found _ debugTypesModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug.Internal.Types") Nothing

  Ghc.Found _ debugModule <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug") Nothing

  debugPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "Debug")
  debugKeyPredName <- Ghc.lookupOrig debugTypesModule (Ghc.mkClsOcc "DebugKey")
  entryName <- Ghc.lookupOrig debugModule (Ghc.mkVarOcc "entry")

  -- find all uses of debug predicates in type signatures
  let nameMap =
        Syb.everything M.union
          (Syb.mkQ mempty $ sigUsesDebugPred debugPredName debugKeyPredName)
          hsGroup

  -- Find the functions corresponding to those signatures and modify their definition.
  binds' <-
    Syb.mkM (modifyBinding nameMap entryName)
      `Syb.everywhereM` binds

  tyClGroups' <-
    traverse
      (modifyTyClGroup debugPredName debugKeyPredName entryName)
      tyClGroups

  pure (tcGblEnv, hsGroup { Ghc.hs_valds = Ghc.XValBindsLR $ Ghc.NValBinds binds' sigs
                          , Ghc.hs_tyclds = tyClGroups'
                          })
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

-- TODO as part of the let binding placed in the body of a function, should
-- emit an event for entry into a function. We need to know about function ids
-- even if no trace is used within the body.

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

-- | Used to add the let binding to where binds
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

modifyTyClGroup
  :: Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.TyClGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TyClGroup Ghc.GhcRn)
modifyTyClGroup debugName debugKeyName entryName
-- TODO modify default implementations
  g@Ghc.TyClGroup{ Ghc.group_instds = instances, Ghc.group_tyclds = tyCls } = do
    let modifyInstance c@Ghc.ClsInstD{ Ghc.cid_inst = inst } = do
          inst' <-
            modifyClsInstDecl tyClNameMap debugName debugKeyName entryName inst
          pure c { Ghc.cid_inst = inst' }
    instances' <- (traverse . traverse) modifyInstance instances
    pure g { Ghc.group_instds = instances' }
  where
    maybeClassDecl c@Ghc.ClassDecl{} = Just c
    maybeClassDecl _ = Nothing
    -- why is this not finding anything?
    tyClNameMap =
      foldMap
          ( foldMap (sigUsesDebugPred debugName debugKeyName . Ghc.unLoc)
          . Ghc.tcdSigs
          )
      $ mapMaybe (maybeClassDecl . Ghc.unLoc) tyCls


-- | Modify bindings for a type class instance declaration. If there are instance
-- sigs, they override entries in the map generated from the type class decl.
-- This map will be empty if the class is not defined beside the instances,
-- therefore instance sigs are necessary to ensure that debugging occurs on
-- class method invocations.
modifyClsInstDecl
  :: M.Map Ghc.Name (Maybe Ghc.FastString)
  -> Ghc.Name -- ^ Debug name
  -> Ghc.Name -- ^ DebugKey name
  -> Ghc.Name -- ^ entry name
  -> Ghc.ClsInstDecl Ghc.GhcRn
  -> Ghc.TcM (Ghc.ClsInstDecl Ghc.GhcRn)
modifyClsInstDecl nameMap debugName debugPredName entryName
    c@Ghc.ClsInstDecl{ Ghc.cid_binds = binds , Ghc.cid_sigs = sigs }
      = do
  let overrides =
        foldMap (sigUsesDebugPred debugName debugPredName . Ghc.unLoc)
                sigs
      nameMap' = overrides <> nameMap

  newBinds <-
    Syb.mkM (modifyBinding nameMap' entryName)
      `Syb.everywhereM` binds

  pure c { Ghc.cid_binds = newBinds }

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

-- Need to modify let bindings as well. And what going recursively into where
-- and let bindings? a where binding can have its own where clause etc.
-- Could do a syb recursion that stops when a function is found that we have
-- a name in the map for.
-- Let bindings don't matter
-- What about where clauses of let bindings? Will need to recurse into GRH
-- as well as local binds? Yes

emitEntryEvent
  :: Ghc.Name
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
emitEntryEvent emitEntryName (Ghc.GRHS x guards body) =
  Ghc.GRHS x guards . Ghc.noLoc $
    Ghc.HsApp Ghc.NoExtField
      (Ghc.noLoc . Ghc.HsVar Ghc.NoExtField $ Ghc.noLoc emitEntryName)
      body

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
isDebuggerIpCt _ = False

tcPluginSolver :: Ghc.TcPluginSolver
tcPluginSolver [] [] wanted = do
  --Ghc.tcPluginIO . putStrLn $ ppr wanted
  case filter isDebuggerIpCt wanted of

    [w]
      | Ghc.IPOccOrigin _ <- Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
      -> do
        --Ghc.tcPluginIO . putStrLn . ppr $ Ghc.ctl_origin . Ghc.ctev_loc $ Ghc.cc_ev w
        pure $ Ghc.TcPluginOk [] []
      | otherwise
      -> do
           let expr = Ghc.mkNothingExpr Ghc.anyTy
           pure $ Ghc.TcPluginOk [(Ghc.EvExpr expr, w)] []
    _ -> pure $ Ghc.TcPluginOk [] []
tcPluginSolver _ _ _ = pure $ Ghc.TcPluginOk [] []
