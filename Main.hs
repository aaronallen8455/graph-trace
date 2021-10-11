{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE DataKinds #-}

import           GHC.Stack

import Debug

main :: IO ()
main = do
  --let ?_debug_ip = Just (Nothing, "insert")
  test
  test


-- test :: (?_debug_ip :: (Maybe String, String)) => IO ()
-- test = test2

test :: DebugKey "blah" => IO ()
test = do
  trace
  trace
  another
  another

another :: Debug => IO ()
another = trace

-- test :: (?x :: String) => IO ()
-- test = print ?x

