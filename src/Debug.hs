{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Debug where

import           Control.Applicative ((<|>))
import           Data.Foldable
import           Data.Traversable
import           Data.IORef
import qualified Data.Set as S
import           Data.Maybe

import qualified GHC.Builtin.Names as Ghc
import qualified GHC.Builtin.Types as Ghc
import qualified GHC.Core as Ghc
import qualified GHC.Core.Class as Ghc
import qualified GHC.Core.Make as Ghc
import qualified GHC.Core.Type as Ghc
import qualified GHC.Core.Utils as Ghc
import qualified GHC.Data.FastString as Ghc
import qualified GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import qualified GHC.Driver.Types as Ghc
import qualified GHC.Hs.Expr as Ghc
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Tc.Plugin as Ghc
import qualified GHC.Tc.Types as Ghc
import qualified GHC.Tc.Types.Constraint as Ghc
import qualified GHC.Tc.Types.Evidence as Ghc
import qualified GHC.Types.Basic as Ghc
import qualified GHC.Types.Id as Ghc
import qualified GHC.Types.Name as Ghc hiding (varName)
import qualified GHC.Types.Name.Occurrence as Ghc hiding (varName)
import qualified GHC.Types.SrcLoc as Ghc
import qualified GHC.Types.Unique.Supply as Ghc
import qualified GHC.Types.Var as Ghc
import qualified GHC.Utils.Outputable as Ghc

import           Data.Generics (everywhereM, mkM)

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.pluginRecompile = Ghc.purePlugin
    , Ghc.tcPlugin = \_ -> Just tcPlugin
    , Ghc.typeCheckResultAction = const typeCheckResultAction
    }

typeCheckResultAction :: Ghc.ModSummary -> Ghc.TcGblEnv -> Ghc.TcM Ghc.TcGblEnv
typeCheckResultAction _modSummary tcGblEnv = do
  x <- mkM test `everywhereM` Ghc.tcg_binds tcGblEnv
  pure tcGblEnv

test :: Ghc.LHsExpr Ghc.GhcTc -> Ghc.TcM ( Ghc.LHsExpr Ghc.GhcTc )
test = undefined

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = Ghc.tcPluginIO $ newIORef False
    , Ghc.tcPluginStop = \_ -> pure ()
    , Ghc.tcPluginSolve = tcPluginSolver
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

-- I'll be able to know how many times the IP constraint will appear for each
-- function? No because the user controls where the traces are used.
-- Actually will have some knowledge of which function it is occurring for
-- because there will also be a wanted for the debug label constraint (or tf)

tcPluginSolver :: IORef Bool -> Ghc.TcPluginSolver
tcPluginSolver givenHandledRef given derived wanted = do
  firstGivenHandled <- Ghc.tcPluginIO $ readIORef givenHandledRef

  case ( filter isDebuggerIpCt given
       , filter isDebuggerIpCt wanted
       ) of
    ([g], []) -> do
      Ghc.tcPluginIO $ putStrLn "case 1"
      let ev = Ghc.ctEvTerm $ Ghc.cc_ev g
      Ghc.tcPluginIO $ writeIORef givenHandledRef True
      pure $ if firstGivenHandled
         then Ghc.TcPluginOk [] []
         else Ghc.TcPluginOk [(ev, g)] [g] -- this can also be [] []!

    ([g], [w]) -> do
      Ghc.tcPluginIO $ putStrLn "case 2"

      let ev = Ghc.cc_ev g
          prevExpr = Ghc.ctEvExpr ev

      tupFstUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
      let tupFstName = Ghc.mkSystemVarName tupFstUniq "a"
          tupFstTy = Ghc.mkTyConApp Ghc.maybeTyCon [Ghc.stringTy]
          tupFstId = Ghc.mkLocalId tupFstName Ghc.Many tupFstTy

      tupSndUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
      let tupSndName = Ghc.mkSystemVarName tupSndUniq "b"
          tupSndTy = Ghc.stringTy
          tupSndId = Ghc.mkLocalId tupSndName Ghc.Many tupSndTy

      tupUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
      let tupName = Ghc.mkSystemVarName tupUniq "c"
          tupTy = Ghc.mkTupleTy Ghc.Boxed [tupFstTy, tupSndTy]
          tupId = Ghc.mkLocalId tupName Ghc.Many tupTy

      let x = case prevExpr of
                Ghc.Var i ->
                  let n = Ghc.mkClonedInternalName tupUniq $ Ghc.varName i
                   in Ghc.Var $ Ghc.setVarName i n

      let ip_co = Ghc.unwrapIP (Ghc.exprType prevExpr)
          castedPrevExpr = Ghc.Cast prevExpr ip_co

      let mPrevStr = Ghc.mkJustExpr Ghc.stringTy
                   . Ghc.mkTupleSelector [tupFstId, tupSndId] tupSndId tupId
                   $ castedPrevExpr

      newStr <- Ghc.unsafeTcPluginTcM $ Ghc.mkStringExpr "inserted2"
      let newTup = Ghc.mkCoreTup [mPrevStr, newStr]

      pure $ Ghc.TcPluginOk [(Ghc.EvExpr newTup, w)] []

    ([], [w]) -> do
      Ghc.tcPluginIO $ putStrLn "case 3"
      str <- Ghc.unsafeTcPluginTcM $ Ghc.mkStringExpr "inserted"
      let tuple = Ghc.mkCoreTup [Ghc.mkNothingExpr Ghc.stringTy, str]
      pure $ Ghc.TcPluginOk [(Ghc.EvExpr tuple, w)] []

    ([], []) -> do
      Ghc.tcPluginIO $ putStrLn "case 4"
      pure $ Ghc.TcPluginOk [] []

    _ -> do
      Ghc.tcPluginIO $ putStrLn "unexpected givens/wanteds"
      pure $ Ghc.TcPluginOk [] []

--   ys <- fmap catMaybes . for given $ \ct -> do
--       let ev = Ghc.cc_ev ct
--           prevExpr = Ghc.ctEvExpr ev
-- 
--       tupFstUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
--       let tupFstName = Ghc.mkSystemVarName tupFstUniq "a"
--           tupFstTy = Ghc.mkTyConApp Ghc.maybeTyCon [Ghc.stringTy]
--           tupFstId = Ghc.mkLocalId tupFstName Ghc.Many tupFstTy
-- 
--       tupSndUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
--       let tupSndName = Ghc.mkSystemVarName tupSndUniq "b"
--           tupSndTy = Ghc.stringTy
--           tupSndId = Ghc.mkLocalId tupSndName Ghc.Many tupSndTy
-- 
--       tupUniq <- Ghc.unsafeTcPluginTcM Ghc.getUniqueM
--       let tupName = Ghc.mkSystemVarName tupUniq "c"
--           tupTy = Ghc.mkTupleTy Ghc.Boxed [tupFstTy, tupSndTy]
--           tupId = Ghc.mkLocalId tupName Ghc.Many tupTy
-- 
--       let x = case prevExpr of
--                 Ghc.Var i ->
--                   let n = Ghc.mkClonedInternalName tupUniq $ Ghc.varName i
--                    in Ghc.Var $ Ghc.setVarName i n
-- 
--       let ip_co = Ghc.unwrapIP (Ghc.exprType prevExpr)
--           castedPrevExpr = Ghc.Cast prevExpr ip_co
-- 
--       let mPrevStr = Ghc.mkJustExpr Ghc.stringTy
--                    . Ghc.mkTupleSelector [tupFstId, tupSndId] tupSndId tupId
--                    $ castedPrevExpr
-- 
--       newStr <- Ghc.unsafeTcPluginTcM $ Ghc.mkStringExpr "inserted2"
--       let newTup = Ghc.mkCoreTup [mPrevStr, newStr]
-- 
--       Ghc.tcPluginIO $ putStrLn (ppr newTup)
--       Ghc.tcPluginIO $ writeIORef s (Just $ Ghc.EvExpr newTup)
--       --pure $ Just (Ghc.ctEvTerm $ Ghc.cc_ev ct, ct)
--       pure $ Just (Ghc.EvExpr newTup, ct)
--       --ppr (Ghc.ctev_evar $ Ghc.cc_ev ct)
-- 
--   xs <- for wanted $ \ct -> do
--     case ct of
--       Ghc.CDictCan{} -> do
--         Ghc.tcPluginIO $ putStrLn $ Ghc.showSDocUnsafe
--           $ Ghc.ppr $ Ghc.cc_ev ct
-- --         Ghc.tcPluginIO $ putStrLn "CDictCan"
-- 
--         -- Can easily construct a string, but how can I do an unsafePerformIO
--         -- that generates a random thing?
--         -- mkCoreApps :: CoreExpr -> [CoreExpr] -> CoreExpr
-- 
-- --         -- | Parses a string as an identifier, and returns the list of 'Name's that
-- -- -- the identifier can refer to in the current interactive context.
-- -- parseName :: GhcMonad m => String -> m [Name]
-- -- parseName str = withSession $ \hsc_env -> liftIO $
-- 
-- -- -- | Is this a symbol literal. We also look through type synonyms.
-- -- isStrLitTy :: Type -> Maybe FastString
-- 
-- --         pushCSVar <- lookupId pushCallStackName
-- --         mkCoreApps (Var pushCSVar) [...]
--         str <- Ghc.unsafeTcPluginTcM $ Ghc.mkStringExpr "inserted"
--         let tuple = Ghc.mkCoreTup [Ghc.mkNothingExpr Ghc.stringTy, str]
--         case ys of
--           [] -> pure (Ghc.EvExpr tuple, ct)
--           [(last, _)] -> do
--             Ghc.tcPluginIO $ putStrLn "........."
--             pure (last, ct)
-- 
--   case mLast of
--     Nothing -> do
--       Ghc.tcPluginIO $ putStrLn "NOTHING"
--       pure $ Ghc.TcPluginOk (xs ++ ys) (snd <$> ys)
--     Just _ -> do
--       Ghc.tcPluginIO $ putStrLn "JUST"
--       pure $ Ghc.TcPluginOk xs []

-- the winning strategy seems to be to put the givens into both outputs only
-- on the first time, then all other times simply deal with the wanteds.
-- Eventually there will be a round with both a given and a wanted and we can
-- then construct the desired value and use if for the wanted constraint.
-- Therefore we only need to keep track of a boolean state.

-- data TcPluginResult
--   = TcPluginContradiction [Ct]
--     -- ^ The plugin found a contradiction.
--     -- The returned constraints are removed from the inert set,
--     -- and recorded as insoluble.
-- 
--   | TcPluginOk [(EvTerm,Ct)] [Ct]
--     -- ^ The first field is for constraints that were solved.
--     -- These are removed from the inert set,
--     -- and the evidence for them is recorded.
--     -- The second field contains new work, that should be processed by
--     -- the constraint solver.
--
-- -- An EvTerm is, conceptually, a CoreExpr that implements the constraint.
-- -- Unfortunately, we cannot just do
-- --   type EvTerm  = CoreExpr
-- -- Because of staging problems issues around EvTypeable
-- data EvTerm
--   = EvExpr EvExpr
-- 
--   | EvTypeable Type EvTypeable   -- Dictionary for (Typeable ty)
-- 
--   | EvFun     -- /\as \ds. let binds in v
--       { et_tvs   :: [TyVar]
--       , et_given :: [EvVar]
--       , et_binds :: TcEvBinds -- This field is why we need an EvFun
--                               -- constructor, and can't just use EvExpr
--       , et_body  :: EvVar }
-- 
--   deriving Data.Data
-- 
-- type EvExpr = CoreExpr

--   = CDictCan {  -- e.g.  Num xi
--       cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
-- 
--       cc_class  :: Class,
--       cc_tyargs :: [Xi],   -- cc_tyargs are function-free, hence Xi
-- 
--       cc_pend_sc :: Bool   -- See Note [The superclass story] in GHC.Tc.Solver.Canonical
--                            -- True <=> (a) cc_class has superclasses
--                            --          (b) we have not (yet) added those
--                            --              superclasses as Givens
--     }

