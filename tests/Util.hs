
-------------------------------------------------------------------------------

{-  OPTIONS_GHC -O0 #-}  -- if want quick recompln.
{-  OPTIONS_GHC -package ghc #-}  -- (done via command-line/cabal)
{-# LANGUAGE CPP #-}

#define USE_ANSI_COLOUR_TERMINAL 0

-------------------------------------------------------------------------------

  module Util (

    show_list ,
    trace ,
    trace_ ,
    trace' ,
    myTrace ,
    myTrace' ,
    spoor ,
    spoor' ,
    spoorf ,
#if 0
    spoorNF ,
#endif
    trayceIO ,

  ) where

-------------------------------------------------------------------------------

  import System.IO

  import Data.List ( intersperse )

#if 0
  import Control.DeepSeq.Generics ( genericRnf )
  import Control.DeepSeq.Generics ( force )
  import Control.DeepSeq.Generics ( NFData(..) )
#endif

  import System.IO.Unsafe ( unsafePerformIO )  -- XXX for tracing only

-------------------------------------------------------------------------------

  show_list lst = concat $ intersperse "\n" $ map show lst

-------------------------------------------------------------------------------

  {-# NOINLINE trace #-}
  trace :: String -> a -> a
  trace s a = x `seq` a
   where x = unsafePerformIO $ do hPutStr stdout (s++"\n") ; hFlush stdout

  {-# NOINLINE trace_ #-}
  trace_ :: String -> a -> a
  trace_ s a = a

  {-# NOINLINE trace' #-}
  trace' :: String -> a -> a
  trace' s a = x `seq` a
   where x = unsafePerformIO $ do hPutStr stdout s ; hFlush stdout

  {-# NOINLINE myTrace #-}
  myTrace :: String -> a -> a
  myTrace s a = x `seq` a
   where x = unsafePerformIO $ do hPutStr stderr (s++"\n") ; hFlush stderr

  {-# NOINLINE myTrace' #-}
  myTrace' :: String -> a -> a
  myTrace' s a = x `seq` a
   where x = unsafePerformIO $ do hPutStr stderr s ; hFlush stderr

  -- These work reasonably well, but sometimes string's
  -- pile up, and then show expr's follow.  spoorNF is
  -- provided for these cases...

  {-# NOINLINE spoor #-}
  spoor :: Show a => String -> a -> a
  spoor string expr = unsafePerformIO $ do
    trayceIO $ string ++ show expr ++ "\n"
    return expr

  {-# NOINLINE spoor' #-}
  spoor' :: Show a => String -> a -> a
  spoor' string expr = unsafePerformIO $ do
    trayceIO $ string ++ show expr
    return expr

  {-# NOINLINE spoorf #-}
  spoorf :: Show b => String -> (a -> b) -> a -> a
  spoorf string f expr = unsafePerformIO $ do
    trayceIO $ string ++ show (f expr) ++ "\n"
    return expr

#if 0
  {-# NOINLINE spoorNF #-}
  spoorNF :: (Show b, NFData b) => String -> (a -> b) -> a -> a
  spoorNF string f expr = unsafePerformIO $ do
    let ss = string ++ show (f expr)
    trayceIO $ force ss
    return expr
#endif

  -- (There is no NOINLINE on this in Common.Trace.  And the
  -- code was templated from the Debug.Trace library.)
  trayceIO :: String -> IO ()
  trayceIO msg = do
    hPutStr stderr msg

-------------------------------------------------------------------------------

