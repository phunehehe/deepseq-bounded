
{-# LANGUAGE CPP #-}
{-  DeriveDataTypeable, DeriveGeneric #-}
--- {-  LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric #-}

  module Main (main) where

#if 0
-- IUT
  import Control.DeepSeq.Bounded
#if ! HASKELL98_FRAGMENT
  import Control.DeepSeq.Bounded.Generic
#endif
#endif

  import Test.HUnit
  import System.Exit
  import System.IO ( stdout )
  import Debug.Trace ( trace )

  import qualified Tests

--------------------------------------------

  tests =
    "All" ~: [ Tests.tests
             ]

  main = do
            putStrLn "Running tests for deepseq-bounded..."
            counts <- runTestTT tests
            if failures counts > 0
              then exitFailure
              else exitSuccess

