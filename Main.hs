{-# OPTIONS_GHC -fplugin=Debug #-}
{-# LANGUAGE DataKinds #-}

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
another = trace "another" pure ()

andAnother :: Debug => IO ()
andAnother = trace "hello!" pure ()

-- test :: (?x :: String) => IO ()
-- test = print ?x

