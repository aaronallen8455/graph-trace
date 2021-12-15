{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Graph.Trace.Internal.Predicates
  ( removeConstraints
  , addConstraintToSig
  ) where

import           Control.Monad.Trans.Writer.CPS
import qualified Data.Generics as Syb
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S

import qualified Graph.Trace.Internal.GhcFacade as Ghc
import           Graph.Trace.Internal.Types

-- | Removes debug predicates from the type signatures in an expression.
-- This is necessary if there are type signatures for pattern bound names and
-- the monomorphism restriction is on.
removeConstraints :: Syb.Data a => DebugNames -> S.Set Ghc.Name -> a -> a
removeConstraints debugNames targetNames thing
  | S.null targetNames = thing
  | otherwise = Syb.mkT processBind `Syb.everywhere` thing
  where
    processBind :: Ghc.HsValBinds Ghc.GhcRn -> Ghc.HsValBinds Ghc.GhcRn
    processBind (Ghc.XValBindsLR (Ghc.NValBinds binds sigs)) =
      Ghc.XValBindsLR (Ghc.NValBinds binds (concatMap removeConstraint sigs))
    processBind binds = binds
    removeConstraint (Ghc.L loc (Ghc.TypeSig x1 names sig)) =
      let (targeted, inert) =
            L.partition ((`S.member` targetNames) . Ghc.unLoc) names
       in [ Ghc.noLocA' . Ghc.TypeSig x1 targeted
              $ Syb.mkT removePred `Syb.everywhere` sig
          , Ghc.L loc $ Ghc.TypeSig x1 inert sig
          ]
    removeConstraint s = [s]
    removePred (Ghc.HsQualTy' x ctx body) =
      let newCtx = (fmap . fmap) (filter (notDebugPred . Ghc.unLoc)) ctx
       in Ghc.HsQualTy' x newCtx body
    removePred x = x
    notDebugPred = isNothing . checkForDebugPred debugNames

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
            -- Note that DebugMuted bindings should still be included because
            -- the muted status needs to be inherited by the functions called from it
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

-- | Check if a type has a debug predicate in it's context. If so, return the
-- override key if supplied and the propagation strategy.
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
