{-# OPTIONS_GHC -fplugin=Debug #-}
--{-# OPTIONS_GHC -ddump-rn-ast #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

import Debug
import           Class

main :: Debug => IO ()
main = do
  test
  andAnother
  test

test :: DebugKey "blah" => IO ()
test = do
  andAnother
  trace "test" pure ()
  putStrLn $ deff (I 3)
  putStrLn $ classy (I 4)
  putStrLn $ classier (I 5)
  inWhere
  let inLet :: Debug => IO ()
      inLet = do
        letWhere
        another
          where letWhere = trace "hello" pure ()
  inLet
  another
  trace "leaving" pure ()
    where
      inWhere :: Debug => IO ()
      inWhere = do
        innerWhere
          where innerWhere = trace "innerWhere" pure ()

another :: Debug => IO ()
another
  | trace "another" True = pure ()
  | otherwise = pure ()

andAnother :: Debug => IO ()
andAnother = trace "hello!" pure ()

newtype I = I Int deriving Show

instance Classy I where
  classy = boo
    where
      boo :: Debug => I -> String
      boo = trace "boohoo" show

instance Classier I where
  classier = show

-- test :: (?x :: String) => IO ()
-- test = print ?x

