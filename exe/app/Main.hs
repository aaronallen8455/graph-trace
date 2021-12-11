--{-# OPTIONS_GHC -fplugin=Debug -fplugin-opt Debug:debug-all #-}
{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
-- {-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
--{-# OPTIONS_GHC -ddump-rn-ast #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad
import           Control.Concurrent
import           Data.Functor.Identity (Identity(..))
import Graph.Trace
import Class

import qualified System.Random as Rand
import           System.IO.Unsafe

-- main :: IO ()
-- main = let ?x = "testing" in print test
-- --   where
-- --     test :: ()
-- --     test =
-- --       let x = ?x
-- --        in x
-- 
-- test :: (?x :: String) => String
-- test = tt where
--   tt :: String
--   tt = ?x

main :: DebugDeep => IO ()
main = trace bah print unassuming >> buzzard
  where
    --unassuming :: Either Bool Int
    -- thisIsABoolean :: Bool
    unassuming@(Left True) =
      trace bah $! (Left True :: Either Bool Int)

    buzzard = do
      putStrLn "please, help"
      traceM bah

    bah :: String
    bah = unsafePerformIO $ do
      let thing = ?_debug_ip
      l <- getLine
      pure $ l <> show (propagation <$> thing)

--         where
--           inFlight = putStrLn "need help now"
         -- shitty = ()
  --replicateM_ 2 $ forkIO test
--   andAnother
--   test'

-- test' :: IO ()
-- test' = do
--   andAnother
--   trace "test\ntest" pure ()
--   traceM "yo"
--   putStrLn $ deff (I 3)
--   x <- readLn
--   case x of
--     3 -> putStrLn $ classy (I x)
--     _ -> pure ()
--   putStrLn $ classier (I 5)
--   inWhere
--   let inLet :: IO ()
--       inLet = do
--         letWhere
--         another
--           where letWhere = trace ("hello" \/& "two") pure ()
--   inLet
--   !_ <- another
--   let letBound = letBoundThing
--   trace letBound pure ()
--   trace "leaving" pure ()
--     where
--       inWhere :: Debug => IO ()
--       inWhere = do
--         innerWhere
--           where
--             innerWhere :: Debug => IO ()
--             innerWhere = trace "innerWhere" pure ()
-- 
-- another :: Debug => IO ()
-- another
--   | trace "another" True = do
--     pure ()
--   | otherwise = pure ()
-- 
-- andAnother :: Debug => IO ()
-- andAnother = trace "hello!" pure ()
-- 
-- letBoundThing :: Debug => String
-- letBoundThing = "bound by let"
-- 
-- (\/&) :: String -> String -> String
-- a \/& b = the a <> ('\\' : b)
-- 
-- the :: a -> a
-- the = id
-- 
-- newtype I = I Int deriving Show
-- 
-- instance Classy I where
--   classy :: Debug => I -> String
--   classy = boo
--     where
--       boo :: Debug => I -> String
--       boo i = trace (show i) "..."
-- 
-- instance Classier I where
--   classier = show
-- 
-- -- test :: (?x :: String) => IO ()
-- -- test = print ?x
-- 
-- data FieldUpdate a
--   = FieldValue a
--   | FieldOmitted
--   | FieldNull
-- 
-- mkUpdater :: f FieldUpdate
--           -> f Maybe
--           -> (forall a. f a -> a x)
--           -> Maybe x
-- mkUpdater update original getField =
--   case getField update of
--     FieldValue a -> Just a
--     FieldOmitted -> getField original
--     FieldNull -> Nothing
-- 
-- data T f =
--   MkT
--     { t1 :: f Bool
--     , t2 :: f String
--     }
-- 
-- zzzz :: T FieldUpdate -> T Maybe -> T Maybe
-- zzzz update orig =
--   let updater :: DebugMute => (forall a. T a -> a x) -> Maybe x
--       updater | let ?x = 1
--         = mkUpdater update orig
--    in MkT
--         { t1 = updater t1
--         , t2 = updater t2
--         }

-- fzzz :: (?_debug_ip :: Maybe DebugIPTy) => T FieldUpdate -> T Maybe -> T Maybe
-- fzzz update orig = entry $
--   let --updater :: (?_debug_ip :: Maybe DebugIPTy)
--       --        => (forall a. T a -> a x) -> Maybe x
--       updater -- | let ?_debug_ip = newIP'
--               = --entry $
--                 mkUpdater update orig
--         where
--           newIP' =
--             let mPrevTag = fmap snd ?_debug_ip
--              in unsafePerformIO $ do
--                     newId <- Rand.randomIO :: IO Word
--                     let newTag = DT
--                           { invocationId = newId
--                           , debugKey = Right "test"
--                           }
--                     pure $ Just (mPrevTag, newTag)
--    in MkT
--      { t1 = updater t1
--      , t2 = updater t2
--      }
--   where
--     newIP =
--       let mPrevTag = fmap snd ?_debug_ip
--        in unsafePerformIO $ do
--               newId <- Rand.randomIO :: IO Word
--               let newTag = DT
--                     { invocationId = newId
--                     , debugKey = Right "test"
--                     }
--               pure $ Just (mPrevTag, newTag)
