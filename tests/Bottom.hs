
  {-# LANGUAGE CPP #-}
  {-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------

  module Bottom where

-------------------------------------------------------------------------------

  import Util

  import Data.Maybe

  import Control.Exception
--import Control.Monad ( guard )
  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )

  import Debug.Trace ( trace )
  import Control.DeepSeq

  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

  __ :: a
--bt'm = undefined
  __ = undefined
--__ = throw BottomedOut

-------------------------------------------------------------------------------

  data BottomedOut = BottomedOut
--data BottomedOut = ThisException | ThatException
    deriving (Show, Typeable)

  instance Exception BottomedOut

-------------------------------------------------------------------------------

