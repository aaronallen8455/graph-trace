{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Graph.Trace.Internal.GhcFacade
  ( module Ghc
  , enumSetMember
  , pattern FunBind'
  , fun_ext'
  , fun_id'
  , fun_matches'
  , pattern HsSig'
  , setSigBody
  , noLocA'
  , emptyEpAnn
  , noLoc'
  , emptyComments'
  , pattern HsQualTy'
  , pattern RealSrcLoc'
  , pattern L'
  ) where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Builtin.Names as Ghc
import GHC.Builtin.Types as Ghc
import GHC.Core.Class as Ghc
import GHC.Core.Make as Ghc
import GHC.Core.Type as Ghc
import GHC.Data.Bag as Ghc
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString as Ghc
import GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import GHC.Driver.Session as Ghc
import GHC.Hs as Ghc hiding (FunDep)
import GHC.Iface.Env as Ghc
import GHC.LanguageExtensions as Ghc hiding (UnicodeSyntax)
import GHC.Rename.Expr as Ghc
import GHC.Tc.Types as Ghc
import GHC.Tc.Types.Constraint as Ghc
import GHC.Tc.Types.Evidence as Ghc
import GHC.Tc.Types.Origin as Ghc
import GHC.Tc.Utils.Monad as Ghc
import GHC.ThToHs as Ghc
import GHC.Types.Basic as Ghc
import GHC.Types.Fixity as Ghc
import GHC.Types.Name as Ghc hiding (varName)
import GHC.Types.SrcLoc as Ghc
import GHC.Types.Unique.Supply as Ghc
import GHC.Unit.Finder as Ghc
import GHC.Unit.Module.Name as Ghc
import GHC.Unit.Types as Ghc
import GHC.Utils.Outputable as Ghc

#elif MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Names as Ghc
import GHC.Builtin.Types as Ghc
import GHC.Core.Class as Ghc
import GHC.Core.Make as Ghc
import GHC.Core.Type as Ghc
import GHC.Data.Bag as Ghc
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString as Ghc
import GHC.Driver.Finder as Ghc
import GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import GHC.Driver.Session as Ghc
import GHC.Hs.Binds as Ghc
import GHC.Hs.Decls as Ghc
import GHC.Hs.Expr as Ghc
import GHC.Hs.Extension as Ghc
import GHC.Hs.Pat as Ghc
import GHC.Hs.Type as Ghc
import GHC.Iface.Env as Ghc
import GHC.LanguageExtensions as Ghc hiding (UnicodeSyntax)
import GHC.Rename.Expr as Ghc
import GHC.Tc.Types as Ghc
import GHC.Tc.Types.Constraint as Ghc
import GHC.Tc.Types.Evidence as Ghc
import GHC.Tc.Types.Origin as Ghc
import GHC.Tc.Utils.Monad as Ghc
import GHC.ThToHs as Ghc
import GHC.Types.Basic as Ghc
import GHC.Types.Name as Ghc hiding (varName)
import GHC.Types.SrcLoc as Ghc
import GHC.Types.Unique.Supply as Ghc
import GHC.Unit.Module.Name as Ghc
import GHC.Unit.Types as Ghc
import GHC.Utils.Outputable as Ghc

#elif MIN_VERSION_ghc(8,10,0)
import Bag as Ghc
import BasicTypes as Ghc
import Class as Ghc
import Constraint as Ghc
import DynFlags as Ghc
import qualified EnumSet as EnumSet
import FastString as Ghc
import Finder as Ghc
import GHC.Hs.Binds as Ghc
import GHC.Hs.Decls as Ghc
import GHC.Hs.Expr as Ghc
import GHC.Hs.Extension as Ghc
import GHC.Hs.Pat as Ghc
import GHC.Hs.Types as Ghc
import GHC.LanguageExtensions as Ghc hiding (UnicodeSyntax)
import GHC.ThToHs as Ghc
import IfaceEnv as Ghc
import MkCore as Ghc
import Module as Ghc
import Name as Ghc
import Outputable as Ghc
import Plugins as Ghc hiding (TcPlugin)
import PrelNames as Ghc
import RnExpr as Ghc
import SrcLoc as Ghc
import TcEvidence as Ghc
import TcOrigin as Ghc
import TcPluginM as Ghc hiding (findImportedModule, getTopEnv, newUnique, getEnvs, lookupOrig)
import TcRnMonad as Ghc
import Type as Ghc
import TysWiredIn as Ghc
import UniqSupply as Ghc
#endif

enumSetMember :: Enum a => a -> EnumSet.EnumSet a -> Bool
enumSetMember = EnumSet.member

pattern FunBind'
  { fun_ext'
  , fun_id'
  , fun_matches'
  } =
#if MIN_VERSION_ghc(9,0,0)
    FunBind fun_ext' fun_id' fun_matches' []
pattern FunBind'
  :: XFunBind GhcRn GhcRn
  -> LIdP GhcRn
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> HsBindLR GhcRn GhcRn
#else
    FunBind fun_ext' fun_id' fun_matches' WpHole []
pattern FunBind'
  :: XFunBind GhcRn GhcRn
  -> Located (IdP GhcRn)
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> HsBindLR GhcRn GhcRn
#endif

pattern HsSig' ty
#if MIN_VERSION_ghc(9,2,0)
  <- L _ (HsSig _ _ ty)
    where
      HsSig' ty = L noSrcSpanA $ HsSig NoExtField (HsOuterImplicit []) ty
pattern HsSig' :: LHsType GhcRn -> LHsSigType Ghc.GhcRn
#else
  <- HsIB _ ty
    where
      HsSig' ty = HsIB [] ty
pattern HsSig' :: LHsType GhcRn -> HsImplicitBndrs GhcRn (LHsType GhcRn)
#endif

setSigBody :: LHsType GhcRn -> LHsSigType GhcRn -> LHsSigType GhcRn
setSigBody body lsig =
#if MIN_VERSION_ghc(9,2,0)
  fmap (\s -> s { sig_body = body }) lsig
#else
  lsig { hsib_body = body }
#endif

noLocA'
#if MIN_VERSION_ghc(9,2,0)
  :: a -> LocatedAn an a
noLocA'
  = noLocA
#else
  :: a -> Located a
noLocA'
  = noLoc
#endif

emptyEpAnn
#if MIN_VERSION_ghc(9,2,0)
  :: EpAnn a
emptyEpAnn
  = noAnn
#else
  :: NoExtField
emptyEpAnn
  = NoExtField
#endif

noLoc'
#if MIN_VERSION_ghc(9,2,0)
  :: a -> a
noLoc' = id
#else
  :: a -> Located a
noLoc' = noLoc
#endif

emptyComments'
#if MIN_VERSION_ghc(9,2,0)
  :: EpAnnComments
emptyComments' = emptyComments
#else
  :: NoExtField
emptyComments' = NoExtField
#endif

pattern HsQualTy'
  :: XQualTy GhcRn
  -> Maybe (LHsContext GhcRn)
  -> LHsType GhcRn
  -> HsType GhcRn
#if MIN_VERSION_ghc(9,2,0)
pattern HsQualTy' x lctx body
  = HsQualTy x lctx body
#else
pattern HsQualTy' x lctx body
  <- HsQualTy x (Just -> lctx) body
    where
      HsQualTy' x Nothing body = HsQualTy x (noLoc []) body
      HsQualTy' x (Just lctx) body = HsQualTy x lctx body
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcLoc' loc <- RealSrcLoc loc _
#else
pattern RealSrcLoc' loc = RealSrcLoc loc
#endif

pattern L' :: SrcSpan -> a
#if MIN_VERSION_ghc(9,2,0)
           -> GenLocated (SrcSpanAnn' ann) a
pattern L' ss a <- L (SrcSpanAnn _ ss) a
#else
           -> Located a
pattern L' ss a <- L ss a
#endif
