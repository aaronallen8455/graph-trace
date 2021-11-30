{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
{-# LANGUAGE MultiWayIf #-}
import           Control.Applicative (empty)
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

import           Graph.Trace

type ItemName = BS8.ByteString
type Pair = (ItemName, ItemName)
type SetIndex = Int
-- A "Set" consists of two groups of items which cannot be bought together.
type ItemToSet = HM.HashMap ItemName (SetIndex, Bool)
-- The Bool indicates which "side" of the Set an item belongs to
type Resolver s = UMV.MVector s (SetIndex, Bool)
-- Sets can combined by mapping the SetIndex of the merged Set to the index of
-- another Set using a mutable vector.
-- The Bool is True if the groups in the Set being merged should be flipped.

main :: DebugDeep => IO ()
main = do
  n <- readLn
  items <- replicateM n BS8.getLine
  m <- readLn
  pairs <- map ((\[x, y] -> (x, y)) . BS8.words) <$> replicateM m BS8.getLine
  case runST $ solve items pairs of
    Nothing -> putStrLn "impossible"
    Just (jesse, walt) -> do
      BS8.putStrLn $ BS8.unwords jesse
      BS8.putStrLn $ BS8.unwords walt

solve :: [ItemName] -> [Pair] -> ST s (Maybe ([ItemName], [ItemName]))
solve items pairs = do
  -- kattis uses an old version of vector w/o generate
  --resolver <- MV.generate 50000 (,False)
  resolver <- MV.new 50000
  for_ [0..49999] $ \i -> MV.write resolver i (i, False)
  mItemToSet <- (`evalStateT` 0) . runMaybeT
              $ foldM (step resolver) HM.empty pairs
  forM mItemToSet $ \itemToSet ->
    foldM (assign resolver itemToSet) ([], []) items

step :: Resolver s -> ItemToSet -> Pair -> MaybeT (StateT SetIndex (ST s)) ItemToSet
step resolver itemToSet (a, b) =
  case (HM.lookup a itemToSet, HM.lookup b itemToSet) of
    (Nothing, Nothing) -> do
      ix <- lift get
      lift $ modify' succ
      pure . HM.insert a (ix, True) $ HM.insert b (ix, False) itemToSet
    (Just (ix, o), Nothing) ->
      pure $ HM.insert b (ix, not o) itemToSet
    (Nothing, Just (ix, o)) ->
      pure $ HM.insert a (ix, not o) itemToSet
    (Just (aix, ao), Just (bix, bo)) -> do
      (aix', ao') <- lift . lift $ resolve resolver aix ao
      (bix', bo') <- lift . lift $ resolve resolver bix bo
      -- compress paths
      let itemToSet' | aix /= aix' || bix /= bix'
                       = HM.insert a (aix', ao')
                       $ HM.insert b (bix', bo') itemToSet
                     | otherwise = itemToSet
      if | aix' == bix', ao' == bo' -> empty -- fail
         | aix' == bix' -> pure itemToSet'
         | otherwise -> do
             lift . lift $ UMV.write resolver bix' (aix', ao' == bo')
             pure itemToSet'

assign :: Resolver s
       -> ItemToSet
       -> ([ItemName], [ItemName])
       -> ItemName
       -> ST s ([ItemName], [ItemName])
assign resolver itemToSet (jesse, walt) item =
  case HM.lookup item itemToSet of
    Nothing -> pure (jesse, item : walt)
    Just (ix, b) -> do
      (_, b') <- resolve resolver ix b
      pure $ if b' then (item : jesse, walt) else (jesse, item : walt)

resolve :: Resolver s -> SetIndex -> Bool -> ST s (SetIndex, Bool)
resolve resolver ix b = do
  (ix', b') <- UMV.read resolver ix
  let b'' = xor b b'
  if ix' == ix
     then pure (ix, b'')
     else resolve resolver ix' b''

