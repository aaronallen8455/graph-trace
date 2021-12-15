{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Graph.Trace.Internal.Instrument
  ( modifyValBinds
  , modifyTyClDecl
  , modifyClsInstDecl
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.CPS
import qualified Data.Generics as Syb
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Haskell.TH as TH
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as Rand

import qualified Graph.Trace.Internal.GhcFacade as Ghc
import           Graph.Trace.Internal.Types

-- | Instrument value bindings that have a signature with a debug pred.
-- This gets applied to both top level bindings as well as arbitrarily nested
-- value bindings.
modifyValBinds
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.NHsValBindsLR Ghc.GhcRn
  -> WriterT
       (S.Set Ghc.Name)
       (StateT (S.Set Ghc.Name) Ghc.TcM)
       (Ghc.NHsValBindsLR Ghc.GhcRn)
modifyValBinds debugNames nameMap (Ghc.NValBinds binds sigs) = do
  binds' <-
    (traverse . traverse)
      (modifyBinds nameMap debugNames)
      binds
  lift $ modify' (S.union $ M.keysSet nameMap)
  pure $ Ghc.NValBinds binds' sigs

-- | Instrument default method implementations in a type class declaration if
-- they contain a Debug pred.
modifyTyClDecl
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.TyClDecl Ghc.GhcRn
  -> WriterT
       (S.Set Ghc.Name)
       (StateT (S.Set Ghc.Name) Ghc.TcM)
       (Ghc.TyClDecl Ghc.GhcRn)
modifyTyClDecl debugNames nameMap
    cd@Ghc.ClassDecl { Ghc.tcdMeths = meths
                     } = do
  newMeths <- modifyBinds nameMap debugNames meths
  pure cd { Ghc.tcdMeths = newMeths }
modifyTyClDecl _ _ x = pure x

-- | Instrument the method implementations in an type class instance if it has
-- a signature containing a debug pred.
modifyClsInstDecl
  :: DebugNames
  -> M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> Ghc.ClsInstDecl Ghc.GhcRn
  -> WriterT
       (S.Set Ghc.Name)
       (StateT (S.Set Ghc.Name) Ghc.TcM)
       (Ghc.ClsInstDecl Ghc.GhcRn)
modifyClsInstDecl debugNames nameMap
    inst@Ghc.ClsInstDecl{ Ghc.cid_binds = binds }
      = do
  newBinds <- modifyBinds nameMap debugNames binds
  pure inst { Ghc.cid_binds = newBinds }
#if !(MIN_VERSION_ghc(9,0,0))
modifyClsInstDecl _ _ x = pure x
#endif

-- | Instrument a set of bindings given a Map containing the names of functions
-- that should be modified.
modifyBinds
  :: M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> DebugNames
  -> Ghc.LHsBinds Ghc.GhcRn
  -> WriterT
       (S.Set Ghc.Name)
       (StateT (S.Set Ghc.Name) Ghc.TcM)
       (Ghc.LHsBinds Ghc.GhcRn)
modifyBinds nameMap debugNames =
  (traverse . traverse)
    (modifyBinding nameMap debugNames)

-- | Instrument a binding if its name is in the Map.
modifyBinding
  :: M.Map Ghc.Name (Maybe Ghc.FastString, Propagation)
  -> DebugNames
  -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
  -> WriterT
       (S.Set Ghc.Name)
       (StateT (S.Set Ghc.Name) Ghc.TcM)
       (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
modifyBinding nameMap debugNames
  bnd@Ghc.FunBind { Ghc.fun_id = Ghc.L' loc name
                  , Ghc.fun_matches = mg@(Ghc.MG _ alts _) }
    | Just (mUserKey, prop) <- M.lookup name nameMap
    = do
      let key = case mUserKey of
                  Nothing -> Left $ Ghc.getOccString name
                  Just k -> Right $ Ghc.unpackFS k

      whereBindExpr <- lift . lift $ mkNewIpExpr loc key prop

      newAlts <- lift $
        (traverse . traverse . traverse)
          (modifyMatch prop whereBindExpr debugNames)
          alts

      pure bnd{Ghc.fun_matches = mg{ Ghc.mg_alts = newAlts }}
modifyBinding nameMap _
  bnd@Ghc.PatBind{ Ghc.pat_lhs = pat } = do
    -- Collect the 'Name's appearing in pattern bindings so that if they have
    -- type signatures, the predicate can be removed if monomorphism
    -- restriction is on.
    let collectName :: Ghc.Pat Ghc.GhcRn -> S.Set Ghc.Name
        collectName = \case
          Ghc.VarPat _ (Ghc.unLoc -> name)
            | M.member name nameMap -> S.singleton name
          Ghc.AsPat _ (Ghc.unLoc -> name) _
            | M.member name nameMap -> S.singleton name
          _ -> mempty
        vars = Syb.everything (<>) (Syb.mkQ mempty collectName) pat
    tell vars
    pure bnd
modifyBinding _ _ bnd = pure bnd

-- | Generate the Name for the where binding
mkWhereBindName :: Ghc.TcM Ghc.Name
mkWhereBindName = do
  uniq <- Ghc.getUniqueM
  pure $ Ghc.mkSystemVarName uniq "new_debug_ip"

-- | Creates a FunBind that will be placed in the where block of a function to
-- serve as the sole definition site of the new DebugContext for that function.
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
  -> DebugNames
  -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> StateT (S.Set Ghc.Name) Ghc.TcM (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn))
modifyMatch prop whereBindExpr debugNames match = do
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

      -- NOINLINE pragma. We don't want the where binding to ever be inlined
      -- because then it would generate a different ID.
      noInlineSig :: Ghc.LSig Ghc.GhcRn
      noInlineSig = Ghc.noLocA' $
        Ghc.InlineSig
          Ghc.emptyEpAnn
          (Ghc.noLocA' whereBindName)
          Ghc.neverInlinePragma

      -- Type sig for 'Maybe DebugContext'
      -- Without an explicit signature for the where binding,
      -- -XNoMonomorphismRestriction causes it to be inlined.
      whereBindSig :: Ghc.LSig Ghc.GhcRn
      whereBindSig = Ghc.noLocA' $
        Ghc.TypeSig
          Ghc.emptyEpAnn
          [Ghc.noLocA' whereBindName] $
            Ghc.HsWC [] $
              Ghc.HsSig' $
                Ghc.noLocA' $
                  Ghc.HsAppTy Ghc.NoExtField
                    (Ghc.noLocA' . Ghc.HsTyVar Ghc.emptyEpAnn Ghc.NotPromoted
                      $ Ghc.noLocA' Ghc.maybeTyConName)
                    (Ghc.noLocA' . Ghc.HsTyVar Ghc.emptyEpAnn Ghc.NotPromoted .
                       Ghc.noLocA' $ debugContextName debugNames
                    )

      -- add the generated bind to the function's where clause
      whereBinds' =
        case whereBinds of
          Ghc.EmptyLocalBinds _ ->
            Ghc.HsValBinds Ghc.emptyEpAnn
              (Ghc.XValBindsLR
                (Ghc.NValBinds [wrappedBind] [noInlineSig, whereBindSig])
              )

          Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) ->
             Ghc.HsValBinds x
               (Ghc.XValBindsLR
                 (Ghc.NValBinds
                   (wrappedBind : binds)
                   (noInlineSig : whereBindSig : sigs)
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
                               else emitEntryEvent (entryName debugNames)
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
#if !(MIN_VERSION_ghc(9,0,0))
    updateMatch x = x
#endif
updateDebugIpInFunBind whereVarName
    b@Ghc.PatBind{ Ghc.pat_rhs = g@Ghc.GRHSs{ Ghc.grhssGRHSs = grhss } }
  = b { Ghc.pat_rhs =
          g{ Ghc.grhssGRHSs = fmap (updateDebugIPInGRHS whereVarName) <$> grhss }
      }
updateDebugIpInFunBind _ b = b

-- | Produce the contents of the where binding that contains the new debug IP
-- value, generated by creating a new ID and pairing it with the old one.
-- The ID is randomly generated. Could instead have a global ID sequence but
-- the random ID has the advantage that a program can be run multiple times
-- using the same log file and the traces won't conflict.
mkNewIpExpr
  :: Ghc.SrcSpan
  -> Either FunName UserKey
  -> Propagation
  -> Ghc.TcM (Ghc.LHsExpr Ghc.GhcRn)
mkNewIpExpr srcSpan newKey newProp = do
  let mDefSite = case Ghc.srcSpanStart srcSpan of
                   Ghc.RealSrcLoc' loc ->
                     Just SrcCodeLoc
                       { srcModule = Ghc.unpackFS $ Ghc.srcLocFile loc
                       , srcLine = Ghc.srcLocLine loc
                       , srcCol = Ghc.srcLocCol loc
                       }
                   _ -> Nothing
  Right exprPs
    <- fmap (Ghc.convertToHsExpr Ghc.Generated Ghc.noSrcSpan)
     . liftIO
     $ TH.runQ [| Just $ mkNewDebugContext mDefSite newKey newProp ?_debug_ip |]

  (exprRn, _) <- Ghc.rnLExpr exprPs

  pure exprRn

-- | Build a new debug context from the previous state. Uses unsafe IO
-- to generate a random ID associated with a particular function invocation
mkNewDebugContext
  :: Maybe DefinitionSite -- ^ Definition site of current function
  -> Either FunName UserKey -- ^ Name of the function or a key supplied by the user
  -> Propagation -- ^ propagation strategy for new context
  -> Maybe DebugContext
  -> DebugContext
mkNewDebugContext mDefSite newKey newProp mPrevCtx =
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
           , definitionSite = mDefSite
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
#if !(MIN_VERSION_ghc(9,0,0))
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
#if !(MIN_VERSION_ghc(9,0,0))
updateDebugIPInGRHS _ x = x
#endif

-- ppr :: Ghc.Outputable a => a -> String
-- ppr = Ghc.showSDocUnsafe . Ghc.ppr
