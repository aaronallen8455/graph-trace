--{-# OPTIONS_GHC -fplugin=Debug -fplugin-opt Debug:trace-all #-}
{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
-- {-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
--{-# OPTIONS_GHC -ddump-rn-ast #-}

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}

import           Control.Monad
import           Control.Concurrent
import           Data.Functor.Identity (Identity(..))
import Graph.Trace
import qualified Debug.Trace as DT
import Class
import           Data.Char
import qualified Data.List as L
import           GHC.Exts

import qualified System.Random as Rand
import           System.IO.Unsafe

pattern Sorted :: (Trace, Ord a) => [a] -> [a]
pattern Sorted xs <- (DT.trace "sort2" mySort -> xs) where
  Sorted xs = DT.trace "sort" mySort xs

mySort :: (Trace, Ord a) => [a] -> [a]
mySort = L.sort

main :: TraceDeep => IO ()
main = do
  firstName <- prompt "Enter your first name"
  lastName <- prompt "Enter your last name"
  let !x = unboxed "test"
  greet firstName lastName

prompt :: String -> IO String
prompt str = do
  traceM str
  putStrLn str
  input <- getLine
  traceM $ "input: " <> input
  pure $ capitalize input

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) =
  let result = toUpper x : map toLower xs
   in trace ("result: " <> result) result

greet :: String -> String -> IO ()
greet first last =
  putStrLn $ "Hello, " <> first <> " " <> last <> "!"

unboxed :: String -> (# Int#, String #)
unboxed _ = (# 3#, "test" #)

{-# NOINLINE main #-}
-- main :: TraceDeep => IO ()
-- main = trace bah print unassuming >> buzzard
--   where
--     unassuming :: Either Bool Int
--     --thisIsABoolean :: Bool
--     unassuming@(Left thisIsABoolean@True) =
--       trace bah $! (Left True :: Either Bool Int)
-- 
--     buzzard = do
--       putStrLn $ "hello" <&> "boo"
--       traceM bah
-- 
--     bah :: String
--     bah = unsafePerformIO $ do
--       getLine
-- (<&>) :: String -> String -> String
-- a <&> b = a

-- main :: Trace => IO ()
-- main = test'

-- test' :: Trace => IO ()
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
--   let inLet :: Trace => IO ()
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
--       inWhere :: Trace => IO ()
--       inWhere = do
--         innerWhere
--           where
--             innerWhere :: Trace => IO ()
--             innerWhere = trace "innerWhere" pure ()
-- 
-- another :: Trace => IO ()
-- another
--   | trace "another" True = do
--     pure ()
--   | otherwise = pure ()
-- 
-- andAnother :: (Trace, Monad m) => m ()
-- andAnother = trace "hello!" pure ()
-- 
-- letBoundThing :: Trace => String
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
--   classy :: Trace => I -> String
--   classy = boo
--     where
--       boo :: Trace => I -> String
--       boo i = trace (show i) "..."
-- 
-- instance Classier I where
--   classier = show
-- -- 
-- -- -- test :: (?x :: String) => IO ()
-- -- -- test = print ?x
-- -- 
data FieldUpdate a
  = FieldValue a
  | FieldOmitted
  | FieldNull

mkUpdater :: f FieldUpdate
          -> f Maybe
          -> (forall a. f a -> a x)
          -> Maybe x
mkUpdater update original getField =
  case getField update of
    FieldValue a -> Just a
    FieldOmitted -> getField original
    FieldNull -> Nothing

data T f =
  MkT
    { t1 :: f Bool
    , t2 :: f String
    }

type TY = forall x. (forall a. T a -> a x) -> Maybe x

-- -- zz :: Int
-- -- zz =
-- --   let x :: [forall x. x -> x]
-- --       x = [id, id]
-- --    in id head x 4
-- 
-- zzz :: Int
-- zzz = id head [1,2,3]

zzzz :: T FieldUpdate -> T Maybe -> T Maybe
zzzz update orig =
  let updater :: TY --(forall a. T a -> a x) -> Maybe x
      updater | let ?x = 1
        = mkUpdater update orig
   in MkT
        { t1 = updater t1
        , t2 = updater t2
        }

fzzz :: Trace => T FieldUpdate -> T Maybe -> T Maybe
fzzz update orig = entry $
  let updater :: -- (?_debug_ip :: Maybe DebugContext)
              (forall a. T a -> a x) -> Maybe x
      updater -- | let ?_debug_ip = newIP'
              = --entry $
                mkUpdater update orig
      addOne = (+1)
   in MkT
     { t1 = updater t1
     , t2 = updater t2
     }
