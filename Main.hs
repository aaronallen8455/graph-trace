{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}

import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Stack

import Debug

main :: IO ()
main = do
  --let ?_debug_ip = "insert"
  test

-- test :: (?_debug_ip :: (Maybe String, String)) => IO ()
-- test = test2

trace :: (?_debug_ip :: String) => IO ()
trace = putStrLn ?_debug_ip

test :: DebugKey "blah" => IO ()
test = do
  trace
  trace
  another

another :: Debug => IO ()
another = trace

-- test :: (?x :: String) => IO ()
-- test = print ?x

st :: DebugKey "..." => IO ()
st = putStrLn "..."
