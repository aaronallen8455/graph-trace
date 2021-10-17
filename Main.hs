{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE DataKinds #-}

import Debug

main :: IO ()
main = do
  test
  putStrLn "end"


test :: DebugKey "blah" => IO ()
test = do
  -- trace "test"
  inWhere
  let inLet :: IO ()
      inLet = do
        letWhere
          where letWhere = trace "hello" pure ()
  inLet
  another
    where
      inWhere :: IO ()
      inWhere = do
        innerWhere
          where innerWhere = trace "innerWhere" pure ()

another :: Debug => IO ()
another = trace "another" pure ()

-- test :: (?x :: String) => IO ()
-- test = print ?x

