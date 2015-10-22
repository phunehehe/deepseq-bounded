
-- XXX This file could be cleaned up a lot, but that's
-- not quite a priority at this moment...

  {-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------

#define USE_TRACE 1
-- This so can get an honest comparison for the user-defined datatypes;
-- if this is 0, the NFDataN instances will be derived via GHC.Generics.
-- (The NFData instances are derived in any case.)
#define USE_MANUAL_INSTANCES 0

-------------------------------------------------------------------------------

  module Foo where

-------------------------------------------------------------------------------

-- TABLE OF EXPECTED AND ACTUAL RESULTS
-- (Only recording "BOT" or "ok".)
--
-- The expressions and getters are effectively:
--
-- expBase1 = ([True,False],3,Just "fox")         :: ([Bool],Int,Maybe String)
-- expBase2 = ([True,False],__,Just "fox")        :: ([Bool],Int,Maybe String)
-- expBase3 = ([True,__],3,Just "fox")            :: ([Bool],Int,Maybe String)
-- expBase4 = ([True,__],3,Just __)               :: ([Bool],Int,Maybe String)
-- expBase5 = ([True,False],3,Just ['f',__,'x'])  :: ([Bool],Int,Maybe String)
-- expBase6 = ([True,False],3,Just __)            :: ([Bool],Int,Maybe String)
-- get1 ((x:_),_,_) = x           -- True
-- get2 (_,x,_) = x               -- 3
-- get3 (_,_,x) = fromJust x      -- "fox"
-- get4 (_,_,Just (x:_)) = x      -- 'f'
--
-- These match like Haskell pattern-matching -- the higher will
-- be matched first; the lower will pick up what trickles through.
--
--  (i,j,k)         Expected           Actual
--   * * 1            ok                 ok
--   2 1 2            BOT                BOT
--   * 1 2            ok                 ok
--   * * 2            BOT                BOT
--   * 4 3            BOT                BOT
--   * 5 3            BOT                BOT
--   * * 3            ok                 ok
--   1 1 4            ok                 ok
--   1 2 4            ok                 ok
--   * * 4            BOT                BOT
--   3 * 5            BOT                BOT
--   * 5 5            BOT                BOT
--   * * 5            ok                 ok

-- AND, SOLVED -- by forcing that show!
-- Great! And it's the USE_EXCEPTION branch too, which I prefer
-- in principle.  Now to consider the output...

-- It seems that this difference decides whether escapee exception
-- happens of (3,1,4) or (3,1,5):
--     doit (1+i) j k $! acc
--  -- doit (1+i) j k $ acc
-- What I don't understand is why "show" which is part of get3
-- doesn't force it where the handler can catch it...

-- What a confusion!
-- So, if you also insert some tracing, you can see all (i,j,k)
-- indices traced, yet the exception still occurs.
-- I suppose this is because the acc string isn't forced
-- until later, and meanwhile the trace lines are free to
-- be printed. (This is the explanation for sure.)
--   But just forcing acc is not the same thing?...
-- If force acc, you see nothing at all printed except the
-- trace lines. All of [ABC](3,1,5) are printed, and then
-- the escapee exception rises.

-- And anyhow, now it passes 4 but not 5, and I don't even know
-- how/why? I'm not using overridden fromJust anymore...
-- (i=3 k=5 exception still escapes in any case!)

-- By overriding fromJust I was able to get past expBase4,
-- but i=3 k=5 exception still escapes!...

-- STILL not getting it -- I can run the gamut, just using
-- putStrLn (as we're in IO anyhow since started fussing
-- with exception catching).  And it says we bottom out
-- a few times, with proper interleaving -- but these are
-- only when /get/ requests a __ value; it doesn't seem
-- to do anything when forcen supposedly forces it.
--   And yet, when run with USE_EXCEPTION, we do see these
-- extra bottom-outs -- unfortunately, the "exceptional
-- exception" occurs and the darned thing doesn't run to
-- completion.

-- Having some problems.
-- Same results if use ErrorCall or use custom BottomedOut,
-- namely, get as far as:
--   ( get3 $ forcen 1 expBase4 )
--   t: BottomedOut
-- The "t: " is significant, since normally when hit
-- the "potholes" my exception handler prints only "BottomedOut".
-- And this also terminates the testing.
-- So clearly, my (own, custom, even -- good to know) exception
-- is escaping the handler.  How can this be?  Esp. as it seems
-- nothing much changes from one test to another.
-- The only thing that comes to mind is, we don't catch ASYNCHRONOUS
-- exceptions this way? Why should an asynch. exception be raised
-- in this case. Trying one more thing: define __ to be trace
-- and just trace the fact it was evaluated...

-- XXX Regarding the use of undefined, I think this is not
-- working well for catching all exceptions? Would we have
-- better luck throwing a custom exception type? How do you
-- do that? "throw BottomedOut" in the component of the tuple?...

  import Data.Maybe

  import Control.Exception
--import Control.Monad ( guard )
  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )

--import Util
  import Debug.Trace ( trace )
  import Control.DeepSeq

#if ! HASKELL98_FRAGMENT
  -- Custom exception requires -XDerivingDataTypeable it seems. [?]
  import Bottom
#endif

  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

#if HASKELL98_FRAGMENT
  __ = undefined
#else
-- imported from Bottom (sorry)
#endif

-------------------------------------------------------------------------------

  expN_1 = [__] :: [Int]
  expN_2 = [0,1,__,3] :: [Int]
  expN_3 = (3.4, [5,__,7], True) :: (Float, [Int], Bool)
  expN_4 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)

  getN_1 xs = show $ ()
--getN_1 xs = show $ head xs
--getN_2 xs = show $ (xs!!1)
  getN_2 xs = show $ (xs!!3)
  getN_3 (_,xs,_) = show $ (xs!!2)

-------------------------------------------------------------------------------

#if 0

  -- Should be able to dodge the third component with a pattern.
--expP_1 = (3.4, [5,__,7], True) :: (Float, [Int], Bool)
  expP_1 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
  patP_1 = Node (TR [typeOf ((__,__,__)::(Float, [Int], Bool))])
             [ Node (TR [typeOf (__::Bool)]) []
--           [ Node (TR [typeOf (__::Float)]) []
--           [ Node (NTR [typeOf (__::Bool)]) []
--           [ Node (NTR [typeOf (__::Float)]) []
--           , Node WS []
             , Node (TW [typeOf ([__]::[Int])]) []
--           , Node (TR [typeOf ([__]::[Int])]) []
--           , Node (TR [typeOf ([__]::[Int])]) [ Node WS [], Node WS [] ]  -- why not?
             , Node WW []
--           , Node I []
             ]
  getP_1 (_,xs,_) = show $ (xs!!2)

  expP_2 = 23 :: Int
--expP_2 = [1,__,3] :: [Int]
  patP_2 = Node WW []
--patP_2 = Node (TR [typeOf ([__]::[Int])]) []
  getP_2 x = show x

#if 1
-- Fuck, this works too... (the only one of all of 'em, except bare 23)
-- See NFDataP.hs for expansion of PatNode constructors...
-- (It's hardly going to be that useful if you have to specify
-- one constructor per node in whole data structure, rather than
-- just match the top bit you want as one expects with patterns.)
-- So I'm basically committed to this expansion...
  expP_3 = (3.4, (5,__), __) :: (Float, (Int,Int), Bool)
  patP_3 = Node (TR [typeOf ((__,__,__)::(Float, (Int,Int), Bool))])
#if 0
             [ Node W []
             , Node I [ Node W [], Node W [] ]  -- works
--           , Node I []                        -- doesn't work
             , Node W []
             ]
#else
             [ Node WR []
             , Node WR [ Node WR [], Node WR [] ]  -- works
--           , Node WR []                        -- doesn't work
             , Node WR []
             ]
#endif
  getP_3 (_,(_,x),_) = show x
#else
  expP_3 = (3.4, (5,__), __) :: (Float, (Int,Int), Bool)
  patP_3 = Node (T [typeOf ((__,__,__)::(Float, (Int,Int), Bool))])
             [ Node (T [typeOf (__::Float)]) []
--           [ Node W []
             , Node (T [typeOf ((__,__)::(Int,Int))]) []
             , Node I []
             ]
  getP_3 (_,(_,x),_) = show x
#endif

#endif

-------------------------------------------------------------------------------

  expBase1 = ([True,False],3,Just "fox")         :: ([Bool],Int,Maybe String)
  expBase2 = ([True,False],__,Just "fox")        :: ([Bool],Int,Maybe String)
  expBase3 = ([True,__],3,Just "fox")            :: ([Bool],Int,Maybe String)
  expBase4 = ([True,__],3,Just __)               :: ([Bool],Int,Maybe String)
  expBase5 = ([True,False],3,Just ['f',__,'x'])  :: ([Bool],Int,Maybe String)
  expBase6 = ([True,False],3,Just __)            :: ([Bool],Int,Maybe String)

#if 1
  get1 ((x:_),_,_) = show x         -- True
  get2 (_,x,_) = show x             -- 3
#if 0
#if 0
  get3 (_,_,x) = do
                    es <- myFromJust x  -- "fox"
                    case es of
                     Left () -> return ""
                     Right s -> return $! show s
--get3 (_,_,x) = show $! myFromJust x  -- "fox"
#else
  get3 (_,_,x) = return $! show $! fromJust x  -- "fox"
--get3 (_,_,x) = return $ show $ fromJust x  -- "fox"
#endif
#else
  get3 ijk (_,_,x) = force $ show $ fromJust x  -- "fox"
--get3 ijk (_,_,x) = trace ("A"++show ijk) ( force $ show $! trace ("B"++show ijk) $ fromJust $! trace ("C"++show ijk) $ x )  -- "fox"
--get3 (_,_,x) = show $! fromJust x  -- "fox"
--get3 (_,_,x) = show $ fromJust x  -- "fox"
#endif
  get4 (_,_,Just (x:_)) = show x    -- 'f'
#else
  get1 ((x:_),_,_) = x       -- True
  get2 (_,x,_) = x           -- 3
  get3 (_,_,x) = fromJust x  -- "fox"
  get4 (_,_,Just (x:_)) = x  -- 'f'
#endif

#if 0
  -- oops, these are just what you DON'T want!
  get1 (_,x,_) = x  -- 3
  get2 (_:x:_) = x  -- False
  get3 (_,_,x) = fromJust x  -- "fox"
  get4 (_,_,(_:x:_)) = x  -- 'o'
#endif

-------------------------------------------------------------------------------

