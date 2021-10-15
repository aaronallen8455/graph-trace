{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

import           GHC.Stack

import Debug

main :: IO ()
main = do
  --let ?_debug_ip = Just (Nothing, "insert")
  test


-- test :: (?_debug_ip :: (Maybe String, String)) => IO ()
-- test = test2

test :: DebugKey "blah" => IO ()
test = do
  trace
  inWhere
  let inLet :: Debug => IO ()
      inLet = do
        trace
  inLet
  another
    where
      inWhere :: Debug => IO ()
      inWhere = do
        trace
        another

another :: Debug => IO ()
another = trace

-- test :: (?x :: String) => IO ()
-- test = print ?x
