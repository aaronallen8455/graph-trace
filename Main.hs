{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE ImplicitParams #-}

import           System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  let ?_debug_ip = (Nothing, "test")
  test

-- test :: (?_debug_ip :: (Maybe String, String)) => IO ()
-- test = test2

test :: (?_debug_ip :: (Maybe String, String)) => IO ()
test = print (?_debug_ip :: (Maybe String, String))

-- test :: (?x :: String) => IO ()
-- test = print ?x

blah :: ()
blah = unsafePerformIO $ putStrLn "test"
