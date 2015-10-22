
-- Refer to /work/Projects/MiniProjects/SybShape/tests for
-- more code in first HUnit example (which itself is still
-- just fudging it)!

{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------

  module Tests ( tests ) where

-------------------------------------------------------------------------------

  import Test.HUnit

  import System.IO.Unsafe ( unsafePerformIO )

#if HASKELL98_FRAGMENT
  import qualified Blah98 as Blah
#else
  import qualified Blah   as Blah
#endif

-------------------------------------------------------------------------------

-- XXX A better way to "fail on purpose" is to have the test
-- return ExitFailure, no?...
--tests = (unsafePerformIO main_tests == output) ~? "FAILING ON PURPOSE TO DISPLAY THE LOGGED OUTPUT!"  -- yeah but it wasn't printed
  tests = unsafePerformIO
            ( do
                 n <- Blah.run_tests
                 putStrLn "FAILING ON PURPOSE TO DISPLAY THE LOGGED OUTPUT!\n"
                 return n )
            ~=? output
  output = 1::Int  -- force test to fail! (so we see the output!)
--output = 0::Int
--output = ()

-------------------------------------------------------------------------------

