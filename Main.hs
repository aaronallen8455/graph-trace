{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin=Debug #-}
{-# OPTIONS_GHC -ddump-rn-ast #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

import Debug

main :: Debug => IO ()
main = do
  test
  andAnother
  test

test :: DebugKey "blah" => IO ()
test = do
  andAnother
  trace "test" pure ()
  putStrLn $ classy (3 :: Int)
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

class Classy a where
  classy :: Debug => a -> String

instance Classy Int where
  classy :: DebugKey "hmm" => Int -> String
  classy | let ?_debug_ip = Nothing = show


-- test :: (?x :: String) => IO ()
-- test = print ?x

