
-------------------------------------------------------------------------------

-- This is a pared-down version of Blah.hs, for testing
-- deepseq-bounded when built with the HASKELL98_FRAGMENT
-- flag set.  See Blah.hs for comments and other "flesh".

-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------

#define FOCUS_TEST 0
#define USE_TRACE 0

-------------------------------------------------------------------------------

  module Blah98 ( run_tests ) where

-------------------------------------------------------------------------------

--import Common
  import Foo

  import Control.DeepSeq.Bounded hiding ( F )

  import Data.Maybe

  import Control.Exception
--import Control.Monad ( guard )
  import Control.Monad ( replicateM )
  import Control.Monad ( when )

  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )

--import Util ( spoor )
  import Debug.Trace ( trace )
  import Control.DeepSeq

  import Data.Typeable ( Proxy(..) )

  import Data.List ( intercalate )

  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

  -- For convenience in GHCi interactive sessions:
  cp = compilePat
--cp = compilePat
  ip = intersectPats
  isp = subPat

-------------------------------------------------------------------------------

  run_tests :: IO Int
  run_tests = do
    putStr "\n"
    -- So can notice the start when scrollback, esp. in ghci reload/reruns.
--  replicateM 50 $ putStrLn "################################################################################"
    putStrLn "\nTesting.\n"

#if ! FOCUS_TEST

#if 1
--  let shp s = s ++ replicate (30 - length s) ' ' ++ show (compilePat s)
    let shp s = 
         unsafePerformIO $
         catch
          ( return $! force $ s ++ replicate (30 - length s) ' ' ++ show (compilePat s) )
          ( \e -> do let err = show (e :: ErrorCall)
                     return err )

    putStrLn "Pattern                       Compiles to"
--  putStrLn $ shp ""  -- empty pattern syntax error
    putStrLn $ shp "*"  -- Node WW []
--  putStrLn $ shp "@"  -- Node WS []  -- obsoleted (use "." w/o children)
    putStrLn $ shp "."  -- Node I []  -- no parent to absorb (should be error)
    putStrLn $ shp "!"  -- Node WS []
--  putStrLn $ shp "**"  -- Node WS []  -- "disconnected pattern (not rooted)"
--  putStrLn $ shp "!*"  -- Node WS []  -- "disconnected pattern (not rooted)"
--  putStrLn $ shp "*!"  -- Node WS []  -- "disconnected pattern (not rooted)"
    putStrLn $ shp "()"  -- Node WR []
    putStrLn $ shp "(*)"  -- Node WR [Node WW []]
    putStrLn $ shp "(**)"  -- Node WR [Node WW [],Node WW []]
    putStrLn $ shp "(!*)"  -- Node WR [Node WS [],Node WW []]
    putStrLn $ shp "(.!)"  -- Node WR [Node I [],Node WS []]
    putStrLn $ shp "(.(.))"  -- Node WR [Node I []] with warning
    putStrLn $ shp "((.))"  -- Node WR [Node WR [Node I []]]
    putStrLn $ shp "(*.*.*.**.*.*)"  -- looks fine
    putStrLn $ shp "(*.(*.(*.)*)*(.*).*)"  -- warning about *{...}
    putStrLn $ shp "(*.(*.(*.)*)*.*)"
--  putStrLn $ shp "(*.(*.(*.)*)(*)(.*).*)"  -- }{ syntax error
-- [old comment for that last:] NodeM WR [NodeM WW [],Nil,NodeM WR [NodeM WW [],Nil,NodeM WR [NodeM WW [],Nil],NodeM WW [],NodeM WW [],Nil,NodeM WW []],Nil,NodeM WW []] -- was wrong but is fixed I think
    putStrLn $ shp "(!*(*23*))"
#endif

#if 1
    let test_patterns10 = [
           (4,  "")  -- syntax error
         , (4,  "!..")  -- syntax error (disconnected pattern)
         , (1,  "(!..)")  -- fail
         , (5,  "(().().())")
         , (2,  "*")
         , (2,  "(***)")
         , (1,  "(...)")
         , (1,  "(*..)")
         , (2,  "(**.)")
         , (2,  "(..*)")

           -- Remember, [a] is a recursive binary ctor app!...
           -- The next four tests give instance errors for [a], but if
           -- testing expTG_* (GNFDataP/grnfp) then it's no longer a list...
         , (13, "(!(*.*).)")
         , (13, "(!(*...).)")
         , (13, "(!(*....).)")
         , (23, "(!(*....)!)")

         , (2,  "(!(*.)*)")
         , (1,  "(!(*.).)")
         , (1,  "(!(*(.*)).)")
         , (2,  "(!(*(*.)).)")
         , (13, "(!((!(.*))).)")  -- See above comment about [a] instance
         , (2,  "(.(!(!(.*))).)")

           -- (See Book, p.84.)
         , (1,  "(!(!(.(!!))).)")
         , (1,  "(!(!(.*)).)")
         , (1,  "(*(*(.*)).)")
         , (2,  "(..*)")
         , (2,  "(*.*)")
         , (1,  ":Tuple3:!(*(*(.*)).)")  -- oops...
         , (1,  ":(,,):!(*(*(.*)).)")
         , (2,  ":(,,):!(*(*(!*)).)")
         , (1,  ":Bool:!(***)")  -- XXX makes no sense (but works) -- oh well;
                                 -- this is an important point to note in
                                 -- docs/blog also...
         , (1,  ":Bool:*")
         , (2,  ":(,,):*")
         , (2,  "(*.!)")
         , (2,  "(*.:Bool:!)")  -- expect parse error
         , (1,  "(*.:Int:!)")  -- still need to implement . when not in type
         , (1,  "(*.:(,):!)")  -- still need to implement . when not in type
         , (1,  "(*.:Int:!)")  -- still need to implement . when not in type
         , (1,  "(*.:Int':!)")  -- still need to implement . when not in type
         , (1,  "(*.:Bool:.)")
         , (2,  "(*.:Int:.)")
         , (2,  "(*.:(,):.)")
         , (1,  "(*.:Bool:.)")
         , (2,  "(*.:Int:.)")
         , (2,  "(*.:(,):.)")
         , (1,  "(**2.)")
         , (2,  "(**3.)")
         , (1,  "(*:G2:.().)")
         , (1,  "(*:Int:.().)")
         ]
    doit14 True test_patterns10
--  doit10 True test_patterns10
#endif

#endif

---------------------------------------------

-- Later: Oh! Don't be silly -- it's all compile-time, and the
-- size or shape of the list is irrelevant -- which rules
-- will fire depends on THIS FILE'S SOURCE CODE (not it's
-- data, including static values)!
-- XXX Even with empty list, the same rule firings occur!...
-- If comment out this block however, the rules do not fire!
#if 1
    -- Testing composition and union:
    let test_patterns11 = [
           (1, "!", "!")
         , (2, "!", "*")
         , (2, "*", "!")
         , (2, "*", "*")
         , (2, "(!!!)", "(*!!)")
         , (2, "(*!!)", "(!!*)")
         , (2, "(!*.)", "(!.!)")
         ]
    doit11 True test_patterns11
#endif

#if 1
    -- Testing splicePats
    let test_patterns12 = [
#if 1
           (1, "!", [], [(0,"*")])
         , (1, "()", [], [(0,"*")])
         , (1, "!", [0], [(0,"*")])
         , (1, "(!!)", [], [(0,"*")])
         , (1, "(!!)", [0], [(0,"*")])
         , (1, "(!!)", [2], [(0,"*")])
         , (1, "(!!)", [], [(0,"*"),(0,".")])
         , (1, "(!!)", [], [(0,"*"),(1,".")])
         , (1, "(!!)", [], [(1,"."),(0,"*")])
         , (1, "(!!)", [], [(0,"*"),(2,".")])
         , (1, "(!!)", [], [(2,"*"),(2,"."),(2,".")])
         , (1, "(!!)", [], [(-1,"*")])
         , (1, "(!!)", [], [(0,"*")])
         , (1, "(!!)", [], [(1,"*")])
         , (1, "(!!)", [], [(2,"*")])
         , (1, "(!!)", [], [(3,"*")])
         , (1, "(!(!!)!)", [1], [(0,"*")])
         , (1, "(!(!!)!)", [1], [(0,"*3"),(1,"."),(2,"*")])
         , (1, "(!(!!)!)", [1,0], [(0,"*"),(0,".")])
         , (1, "((!!!)(()!)!)", [1], [(0,"*"),(0,".")])
         , (1, "((!!!)(()!)!)", [1,0], [(0,"*"),(0,".")])
         , (1, "((...)(().).)", [1,0], [(0,"*"),(0,".")])
         , (1, "(:(,,):!(...)(().).)", [1,0], [(0,"*"),(0,".")])
#else
           (1, "!", "()", [(0,"*")])
         , (1, "()", "()", [(0,"*")])
         , (1, "!", "(())", [(0,"*")])  -- added later, did uncover a bug! (irrefutable pattern failure)
         , (1, "(!!)", "()", [(0,"*")])
         , (1, "(!!)", "(())", [(0,"*")])
         , (1, "(!!)", "(!!())", [(0,"*")])
         , (1, "(!!)", "()", [(0,"*"),(0,".")])
         , (1, "(!!)", "()", [(0,"*"),(1,".")])
         , (1, "(!!)", "()", [(1,"."),(0,"*")])
         , (1, "(!!)", "()", [(0,"*"),(2,".")])
         , (1, "(!!)", "()", [(2,"*"),(2,"."),(2,".")])
         , (1, "(!!)", "()", [(-1,"*")])
         , (1, "(!!)", "()", [(0,"*")])
         , (1, "(!!)", "()", [(1,"*")])
         , (1, "(!!)", "()", [(2,"*")])
         , (1, "(!!)", "()", [(3,"*")])
         , (1, "(!(!!)!)", "(!()!)", [(0,"*")])
         , (1, "(!(!!)!)", "(!()!)", [(0,"*3"),(1,"."),(2,"*")])
         , (1, "(!(!!)!)", "(!(()!)!)", [(0,"*"),(0,".")])
         , (1, "(!(!!)!)", "(!(()))", [(0,"*"),(0,".")])
         , (1, "((!!!)(()!)!)", "(!())", [(0,"*"),(0,".")])
         , (1, "((!!!)(()!)!)", "(!(()))", [(0,"*"),(0,".")])
         , (1, "((...)(().).)", "(!(()))", [(0,"*"),(0,".")])
         , (1, "(:(,,):!(...)(().).)", "(!(()))", [(0,"*"),(0,".")])
#endif
         ]
    doit12 True test_patterns12
#endif

#if ! FOCUS_TEST

#if 1
    -- Testing anything else we can with a single function!
    -- (Getting sick of this cloning.)
    -- Functions are listed in (current) export order.
    -- Things still needing testing are >'d.
    -- Things tested but failing some tests have a *.
    -- 
    -- XXX Wow does this ever look cleaner with "Pat" instead of "Pattern"!...
    -- Fortunately splicePats is already tested separately; the rest
    -- we need to test deal only in Pat args (or list of same), so
    -- we can pass a couple [Pat] to test13 along with an Int code
    -- to control delegation.
    --  1    unionPats       :: [ Pat ] -> Pat
    --  2    intersectPats   :: [ Pat ] -> Pat
    --  3    subPat          :: Pat -> Pat -> Bool
--  --  4    unionPatsStr    :: [ String ] -> String
    --  5    emptyPat        :: Pat
--  --  6    mkPat           :: forall d. Data d => d -> Pat
--  --  7    growPat         :: forall d. Data d => Pat -> d -> Pat
    --  8    shrinkPat       :: Pat -> Pat
    --  9    liftPats        :: [ Pat ] -> Pat
    -- 10    splicePats      :: Pat -> Pat -> [(Int, Pat)] -> Pat
    -- 11    isPath          :: Pat -> Bool
    let test_patterns13 = [
           ( 1, ["(!!!)", "(!.)"], "")
         , ( 2, ["(!!!)", "(!.)"], "")
         , ( 3, ["!", "!"], "True")
         , ( 3, [".", "."], "True")
         , ( 3, ["*", "*"], "True")
         , ( 3, ["!", "()"], "True")
         , ( 3, ["()", "!"], "False")
         , ( 3, ["()", "(!)"], "False")
         , ( 3, ["(!)", "(!)"], "True")
         , ( 3, ["(!)", "(!!)"], "False")
         , ( 3, ["(!)", "((!))"], "True")
         , ( 3, ["(!!)", "(!.)"], "False")
         , ( 3, ["(!.)", "(!!)"], "True")
         , ( 3, ["(!!)", "(!!!)"], "False")
         , ( 3, ["()", "(!!!)"], "False")
         , ( 3, ["!", "(*!.)"], "True")
         , ( 3, [".", "(*!.)"], "True")
         , ( 3, ["*", "(*!.)"], "False")
         , ( 3, ["*", "(*)"], "False")
         , ( 3, ["*", "(**)"], "False")
         , ( 4, ["(!!!)", "(!.)"], "")
         , ( 5, ["(!!!)", "(!.)"], "")
#if 0
         , ( 6, ["(!!!)", "(!.)"], "")
         -- matching against val = ([1,2,3::Int],(False,"foo"))
         -- mkPat val = "((!(!(!!)))(!(!(!(!!)))))"
#endif
#if 0
         , ( 7, ["((!(!!))(!(!!)))"], "")
         , ( 7, ["((!(!(!!)))(!(!(!!))))"], "")
         , ( 7, ["((!(!(!!)))(!(!(!(!!)))))"], "")
#endif
         , ( 8, ["((!(!(!!)))(!(!(!(!!)))))"], "")
         , ( 8, ["((!(!(!!)))(!(!(!!))))"], "")
         , ( 8, ["((!(!!))(!(!!)))"], "")
         , ( 8, ["((.(!!))(.(*3!)))"], "")
         , ( 8, ["((.!)(.!))"], "")
         , ( 8, ["(!!)"], "")
         , ( 8, ["!"], "")
         , ( 9, ["(!!!)", "(!.)"], "")
         , (10, ["(!!!)", "(!.)"], "")
         , (11, ["(!!!)", "(!.)"], "")
         ]
    putStrLn "===================================================="
    putStrLn "Testing miscellaneous NFDataP functions..."
    doit13 True test_patterns13
#endif

#if 1
    -- Testing fusion
    putStrLn "\nTesting fusion\n-fenable-rewrite-rules -O -ddump-rules -ddump-simpl-stats -ddump-rule-firings\n"
    let exp12 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
    let get12 (_,xs,_) = (xs!!2)
    putStrLn $ show $ get12 $ ( forcep ":Bool:*" . forcep ":Int:*" ) exp12
    putStrLn $ show $ get12 $ ( forcep ":Bool:*" . forcep_ (compilePat ":Int:*") ) exp12
    putStrLn $ show $ get12 $ ( forcep_ (compilePat ":Bool:*") . forcep ":Int:*" ) exp12
    putStrLn $ show $ get12 $ ( forcep_ (compilePat ":Bool:*") . forcep_ (compilePat ":Int:*") ) exp12
#endif

#if 1
    putStrLn $ intercalate "\n"
     [ ""
     , "expN_1 = [__] :: [Int]"
     , "expN_2 = [0,1,__,3] :: [Int]"
     , "expN_3 = (3.4, [5,__,7], True) :: (Float, [Int], Bool)"
     , "expN_4 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
     , ""
     , "getN_1 xs = show $ ()"
--   , "getN_1 xs = show $ head xs"
--   , "getN_2 xs = show $ (xs!!1)"
     , "getN_2 xs = show $ (xs!!3)"
     , "getN_3 (_,xs,_) = show $ (xs!!2)"
     ]
    doit5 1 0 1 "" >>= putStrLn
    doit6 1 0 1 "" >>= putStrLn
    doit7 1 0 1 "" >>= putStrLn
    doit8 1 0 1 "" >>= putStrLn
#if 0
    doit9 >>= putStrLn  -- XXX broken (whatever it is)
#endif
#endif

#if 0
    s4 <- doit4 1 0 1 ""
    putStrLn s4
#endif

#if 0
    putStrLn $ ""
           ++ "expBase1 = ([True,False],3,Just \"fox\")         :: ([Bool],Int,Maybe String)\n"
           ++ "expBase2 = ([True,False],__,Just \"fox\")        :: ([Bool],Int,Maybe String)\n"
           ++ "expBase3 = ([True,__],3,Just \"fox\")            :: ([Bool],Int,Maybe String)\n"
           ++ "expBase4 = ([True,__],3,Just __)               :: ([Bool],Int,Maybe String)\n"
           ++ "expBase5 = ([True,False],3,Just ['f',__,'x'])  :: ([Bool],Int,Maybe String)\n"
           ++ "expBase6 = ([True,False],3,Just __)            :: ([Bool],Int,Maybe String)\n"
           ++ "get1 ((x:_),_,_) = x           -- True\n"
           ++ "get2 (_,x,_) = x               -- 3\n"
           ++ "get3 (_,_,x) = fromJust x      -- \"fox\"\n"
           ++ "get4 (_,_,Just (x:_)) = x      -- 'f'\n"
    s1 <- doit1 1 1 1 ""
    putStrLn s1
#endif

#endif

    return 0

-------------------------------------------------------------------------------

  hline :: String
  hline = "----------------------------------------------------\n"
  hdline :: String
  hdline = "====================================================\n"

-------------------------------------------------------------------------------

  doit1 :: Int -> Int -> Int -> String -> IO String
  doit1 i j k acc
   -- this glitch (the exception that seemingly escapes my catch)
   -- happens just for these combos (and for all j):
-- | i == 3 && k == 4  = doit1 (1+i) j k acc  -- trying to avoid a glitch...
-- | i == 3 && k == 5  = doit1 (1+i) j k acc  -- trying to avoid a glitch...
-- | i == 3 && j < 4 && k == 5  = doit1 (1+i) j k acc  -- trying to avoid a glitch...
   | k == 7  = return acc
   | j == 6  = doit1 1 1 (1+k) acc
   | i == 5  = doit1 1 (1+j) k acc
   | otherwise  = do
      fexp <- catch
               -- The semantics of the !'s here are not what they were,
               -- since forcing in some places. [?] But at least the
               -- first one seems definitely still needed...
               -- LATER: The second one is also needed, finally seen!
               -- (It is needed for a version of getE _ = "getee", only.)
               -- XXX I'm just not sure whether the second ! is better
               -- to leave in or leave out...
               ( return $! get $! forcen dep exp )
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
--    doit1 (1+i) j k $! force $ acc
      doit1 (1+i) j k $! acc
--    doit1 (1+i) j k $ acc
        ++ "( get" ++ show i ++ " $ forcen " ++ show dep ++ " expBase" ++ show k ++ " )  "
        ++ fexp ++ "\n"
   where
    exp = case k of
            1 -> expBase1
            2 -> expBase2
            3 -> expBase3
            4 -> expBase4
            5 -> expBase5
            6 -> expBase6
    get = case i of
           1 -> get1
           2 -> get2
--         3 -> unsafePerformIO . get3  -- yeesh!...
--         3 -> get3
           3 -> get3 (i,j,k)
           4 -> get4
    dep = j

#if 0
  -- XXX All this fuss to deal with the case that the
  -- entire argument m is undefined (or whatever is going
  -- for "undefined" at the moment...) -- but, none of
  -- the expBase[1-5] have this! ch.
  myFromJust :: Maybe a -> IO (Either () a)
  myFromJust m = do
                    putStrLn "Boo!"
                    catch
                      (myFromJust' m)
                      (\e -> do putStrLn $! show (e::BottomedOut)
                                putStrLn "HERE!"
                                return $! Left ())
  myFromJust' :: Maybe a -> IO (Either () a)
  myFromJust' Nothing = do
--                         throw BottomedOut
                           return $! Left ()
  myFromJust' (Just x) = return $! Right x
--myFromJust _ = throw BottomedOut
#endif

--------------------------------

#if 0
  doit2 :: Int -> Int -> Int -> String -> IO String
  doit2 i j k acc
-- | k == 2  = {- trace (show (i,j,k)) $ -} return acc
-- | k == 7  = return acc
   | k == 27  = return acc
-- | k == 21  = return acc
-- | k == 5  = return acc
-- | j == 15  = doit2 1 0 (1+k) acc
   | j == 11  = {- trace (show (i,j,k)) $ -} doit2 1 0 (1+k) acc
-- | j == 8  = {- trace (show (i,j,k)) $ -} doit2 1 0 (1+k) acc
-- | j == 3  = {- trace (show (i,j,k)) $ -} doit2 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i,j',k')) $ doit2 1 (1+j) k acc
-- | i == 3  = trace ("End of " ++ show (i,j',k')) $ doit2 1 (1+j) k acc
#else
   | i == 2  = doit2 1 (1+j) k acc
-- | i == 3  = doit2 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
--             ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit2 (1+i) j k $! acc
        ++ "( getB_" ++ show i ++ " $ forcen " ++ show dep ++ (if dep < 10 then " " else "") ++ " expTB_" ++ show (k'-6) ++ " )  "
--      ++ "( getB_" ++ show i ++ " $ forcen " ++ show dep ++ (if dep < 10 then " " else "") ++ " expBase" ++ show k' ++ " )  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            7 -> expBase7
            8 -> expBase8
            9 -> expBase9
            10 -> expBase10
            11 -> expBase11
            12 -> expBase12
            13 -> expBase13
            14 -> expBase14
            15 -> expBase15
            16 -> expBase16
            17 -> expBase17
            18 -> expBase18
            19 -> expBase19
            20 -> expBase20
            21 -> expBase20  -- fudge it!!
            22 -> expBase22
            23 -> expBase23
            24 -> expBase24
            25 -> expBase25
            26 -> expBase26
            27 -> expBase27
            28 -> expBase28
            29 -> expBase29
            30 -> expBase30
            31 -> expBase31
            32 -> expBase32
    get = case i of
           1 -> if k' >= 27 then getB_3 else getB_1
           2 -> getB_2
    dep = j'
    j' = j
--  j' = j+11
--  k' = k+26
    k' = k+6
#endif

#if 0
  doit3 :: Int -> Int -> Int -> String -> IO String
  doit3 i j k acc
   | k == 2  = return acc
   | j == 8  = {- trace (show (i,j,k)) $ -} doit3 1 1 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i,j',k')) $ doit3 1 (1+j) k acc
#else
   | i == 2  = doit3 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit3 (1+i) j k $! acc
        ++ "( getB_" ++ show i ++ " $ forcen " ++ show dep ++ " expBase" ++ show k' ++ " )  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            21 -> expBase21
    get = case i of
           1 -> getA
    dep = j'
    i' = i
    j' = j
    k' = k+20
#endif

#if 0
  doit4 :: Int -> Int -> Int -> String -> IO String
  doit4 i j k acc
-- | k == 2  = return acc
   | k == 4  = return acc
   | j == 8  = {- trace (show (i,j,k)) $ -} doit4 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i,j',k')) $ doit4 1 (1+j) k acc
#else
   | i == 2  = doit4 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit4 (1+i) j k $! acc
        ++ "( getE $ forcen " ++ show dep ++ " expTE_" ++ show k' ++ " )  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            1 -> expTE_1
            2 -> expTE_2
            3 -> expTE_3
    get = case i of
           1 -> getE
    dep = j'
    i' = i
    j' = j
    k' = k
#endif

-------------------------------------------------------------------------------

  doit5 :: Int -> Int -> Int -> String -> IO String
  doit5 i j k acc
   | k == 2  = return acc
   | j == 5  = {- trace (show (i,j,k)) $ -} doit5 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i,j',k')) $ doit5 1 (1+j) k acc
#else
   | i == 2  = doit5 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit5 (1+i) j k $! acc
        ++ "forcen " ++ show dep ++ " expN_" ++ show k' ++ " `seq` ()  =  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            1 -> expN_1
    get = case i of
           1 -> getN_1
    dep = j'
    i' = i
    j' = j
    k' = k

-------------------------------------------------------------------------------

  doit6 :: Int -> Int -> Int -> String -> IO String
  doit6 i j k acc
   | k == 2  = return acc
   | j == 5  = {- trace (show (i,j,k)) $ -} doit6 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i',j',k')) $ doit6 1 (1+j) k acc
#else
   | i == 2  = doit6 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit6 (1+i) j k $! acc
        ++ "getN_" ++ show i' ++" $ forcen " ++ show dep ++ " expN_" ++ show k' ++ "  =  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            _ -> expN_2
    get = case i of
           _ -> getN_2
    dep = j'
    i' = i+1
    j' = j
    k' = k+1

-------------------------------------------------------------------------------

  doit7 :: Int -> Int -> Int -> String -> IO String
  doit7 i j k acc
   | k == 2  = return acc
   | j == 5  = {- trace (show (i,j,k)) $ -} doit7 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i',j',k')) $ doit7 1 (1+j) k acc
#else
   | i == 2  = doit7 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit7 (1+i) j k $! acc
        ++ "getN_" ++ show i' ++" $ forcen " ++ show dep ++ " expN_" ++ show k' ++ "  =  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            _ -> expN_3
    get = case i of
           _ -> getN_3
    dep = j'
    i' = i+2
    j' = j
    k' = k+2

-------------------------------------------------------------------------------

  doit8 :: Int -> Int -> Int -> String -> IO String
  doit8 i j k acc
   | k == 2  = return acc
   | j == 5  = {- trace (show (i,j,k)) $ -} doit8 1 0 (1+k) acc
#if USE_TRACE
   | i == 2  = trace ("End of " ++ show (i',j',k')) $ doit8 1 (1+j) k acc
#else
   | i == 2  = doit8 1 (1+j) k acc
#endif
   | otherwise  = do
      fexp <- catch
               ( return $! get $! forcen dep exp )
               -- ErrorCall if use __ = undefined
               -- BottomedOut if use __ = throw BottomedOut
               (\e -> do let err = show (e :: ErrorCall)
--             (\e -> do let err = show (e :: BottomedOut)
                         return err)
      doit8 (1+i) j k $! acc
        ++ "getN_" ++ show i' ++" $ forcen " ++ show dep ++ " expN_" ++ show k' ++ "  =  "
        ++ fexp ++ "\n"
   where
    exp = case k' of
            _ -> expN_4
    get = case i of
           _ -> getN_3
    dep = j'
    i' = i+2
    j' = j
    k' = k+3

-------------------------------------------------------------------------------

#if 0

#if 0
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
#endif

  doit9 :: IO String
  doit9 = do
#if 0
    putStrLn $ intercalate "\n"
     [ ""
     , "expP_1 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
     , "patP_1 = Node (T [typeOf ((__,__,__)::(Float, [Int], Bool))])"
     , "           [ Node W []"
     , "           , Node (T [typeOf ([__]::[Int])]) []"
     , "           , Node I []"
     , "           ]"
     , "getP_1 (_,xs,_) = show $ (xs!!2)"
     ]
    putStrLn "getP_1 $ forcep patP_1 expP_1"
#endif
    s <- catch
             ( return $! getP_1 $! forcep patP_1 expP_1 )
--           ( return $! getP_2 $! forcep patP_2 expP_2 )
--           ( return $! getP_3 $! forcep patP_3 expP_3 )
             -- ErrorCall if use __ = undefined
             -- BottomedOut if use __ = throw BottomedOut
             (\e -> do let err = show (e :: ErrorCall)
--           (\e -> do let err = show (e :: BottomedOut)
                       return err)
    return s

#endif

-------------------------------------------------------------------------------

  doit10 :: Bool -> [(Int,String)] -> IO ()
  doit10 _ [] = do
                   putStrLn "----------------------------------------------------"
                   return ()
  doit10 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
                  _ -> error $ "doit10: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                 then do
                  putStrLn "===================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "===================================================="
                 else do
                  putStrLn "----------------------------------------------------"
                putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                 then putStrLn $  "expected value           = " ++ expect
                 else return ()
--              putStrLn $ "pat = " ++ show pat
                putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                 then putStrLn $  "expected value           = " ++ expect
                 else return ()
                s1 <- catch
                        ( return $! force $! get $! forcep patstr exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr "actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit10 False t

-------------------------------------------------------------------------------

  doit11 :: Bool -> [(Int,String,String)] -> IO ()
  doit11 _ [] = do
                   putStrLn "----------------------------------------------------"
                   return ()
  doit11 b ((code,patstr1,patstr2):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  _ -> error $ "doit11: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let patstrU = showPat $ unionPats [compilePat patstr1, compilePat patstr2]
                let pat1 = compilePat patstr1
                let pat2 = compilePat patstr2
                let patU = unionPats [pat1,pat2]
                let get (_,xs,_) = show $ (xs!!2)
                if b
                 then do
                  putStrLn "===================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ ( forcep patstr2 . forcep patstr1 ) exp"
--                putStrLn "? = get $ ( forcep ( unionPatsStr [ patstr1, patstr2 ] ) ) exp"
                  putStrLn "? = get $! ( forcep_ ( unionPats [ pat1, pat2 ] ) ) exp"
                  putStrLn "(Results were more interesting with a previous semantics...)."
                  putStrLn "===================================================="
                 else do
                  putStrLn "----------------------------------------------------"
                putStrLn $ "patstr1                     = " ++ patstr1
                putStrLn $ "patstr2                     = " ++ patstr2
                putStrLn $ "patstrU                     = " ++ patstrU
                if code > 4
                 then putStrLn $  "expected value              = " ++ expect
                 else return ()
--              putStrLn $ "pat = " ++ show pat
                putStrLn $ "showPat.compilePat patstr1  = " ++ showPat pat1
                putStrLn $ "showPat.compilePat patstr2  = " ++ showPat pat2
                putStrLn $ "showPat.compilePat patstrU  = " ++ showPat patU
                if code <= 4
                 then putStrLn $  "expected value              = " ++ expect
                 else return ()
                putStr "actual value                = "
                let s1 = get $! ( forcep patstr2 . forcep patstr1 ) exp
                putStrLn s1
                putStr "actual value                = "
                let s2 = get $! forcep patstrU exp
#if 1
                let s3 = s2
#else
                putStrLn s2
                let s3 = get $! forcep_ ( unionPats [ pat1, pat2 ] ) exp
#endif
                return $! s3
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit11 False t

-------------------------------------------------------------------------------

  doit12 :: Bool -> [(Int,String,[Int],[(Int,String)])] -> IO ()
--doit12 :: Bool -> [(Int,String,String,[(Int,String)])] -> IO ()
  doit12 _ [] = do
                   return ()
  doit12 b ((code,patstr1,path,isibs'):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  _ -> error $ "doit12: unexpected code " ++ show code
    s <- catch
           ( do
                let target = compilePat patstr1
--              let path = compilePat patstr2
                let isibs = map (\ (x,y) -> (x,compilePat y)) isibs'
                if b
                 then do
                  putStr hdline
                  putStrLn "Testing splicePats."
                  putStr hline
                 else return ()
                putStrLn $ "target       " ++ patstr1
                putStrLn $ "path         " ++ show path
--              putStrLn $ "path         " ++ patstr2
                putStrLn $ "isibs        " ++ show isibs'
                let s1 = force $ showPat $ splicePats target path isibs
--              let s1 = showPat $! splicePats target path isibs
--              let s1 = showPat $ splicePats target path isibs
                putStr "result       "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    if null t then return () else putStr hline
    doit12 False t

-------------------------------------------------------------------------------

  doit13 :: Bool -> [(Int,[String],String)] -> IO ()
  doit13 _ [] = do
                   putStr hline
                   return ()
  doit13 b ((code,patstrlst,expectstr):t) = do
    s <- catch
           ( do
                s1 <- case code of
-- Where were they done already?
-- Oh, in doit11 and doit12 above...
                       1 -> error $ hline ++ "unionPats test already done test13-1!"
--                     4 -> error $ hline ++ "unionPatsStr test already done test13-4!"
                       10 -> error $ hline ++ "splicePats test already done test13-10!"
                       11 -> error $ hline ++ "isPath test already done test13-11!"
    --  1    unionPats       :: [ Pat ] -> Pat
    --  2  * intersectPats   :: [ Pat ] -> Pat
    --  3  * subPat          :: Pat -> Pat -> Bool
--  --  4    unionPatsStr    :: [ String ] -> String
    --  5  * emptyPat        :: Pat
--  --  6  * mkPat           :: forall d. Data d => d -> Pat
--  --  7  * growPat         :: forall d. Data d => Pat -> d -> Pat
    --  8  * shrinkPat       :: Pat -> Pat
    --  9  * liftPats        :: [ Pat ] -> Pat
    -- 10    splicePats      :: Pat -> Pat -> [(Int, Pat)] -> Pat
    -- 11    isPath          :: Pat -> Bool
                       2 -> do
                               let patA = patstrlst!!0
                               let patB = patstrlst!!1
                               putStr hline
                               putStrLn "Testing      intersectPats [patA, patB]"
                               putStrLn $ "patA         " ++ patA
                               putStrLn $ "patB         " ++ patB
                               putStr $ "result       "
                               let s1 = force $ showPat $ intersectPats [compilePat patA, compilePat patB]
                               return s1
                       3 -> do
                               let patA = patstrlst!!0
                               let patB = patstrlst!!1
                               putStr hline
                               putStrLn "Testing      subPat patA patB"
                               putStrLn $ "patA         " ++ patA
                               putStrLn $ "patB         " ++ patB
                               let s1 = force $ show $ subPat (compilePat patA) (compilePat patB)
                               if s1 /= expectstr
                                then do putStrLn $ "expect       " ++ expectstr
                                        putStr $ "result       "
                                else putStr $ "as expected  "
                               return s1
                       5 -> do
                               putStr hline
                               putStrLn "Testing      emptyPat"
                               putStr $ "result       "
                               let s1 = force $ showPat $ emptyPat
                               return s1
                       6 -> do
#if 0
                               putStr hline
                               putStrLn "Testing      mkPat ([1,2,3],(False,\"foo\"))"
                               putStr $ "result       "
                               let s1 = force $ showPat $ mkPat ([1,2,3::Int],(False,"foo"))
                               return s1
#else
                               return ""
#endif
                       7 -> do
#if 0
                               let patA = patstrlst!!0
                               putStr hline
                               putStrLn "Testing      growPat patA ([1,2,3],(False,\"foo\"))"
                               putStrLn $ "patA         " ++ patA
                               putStr $ "result       "
                               let s1 = force $ showPat $ growPat (compilePat patA) ([1,2,3::Int],(False,"foo"))
                               return s1
#else
                               return ""
#endif
                       8 -> do
                               let patA = patstrlst!!0
                               putStr hline
                               putStrLn "Testing      shrinkPat patA"
                               putStrLn $ "patA         " ++ patA
                               putStr $ "result       "
                               let s1 = force $ showPat $ shrinkPat (compilePat patA)
                               return s1
                       9 -> do
                               putStr hline
                               putStrLn "Testing      liftPats patstrlst"
                               putStrLn $ "patstrs      " ++ show patstrlst
                               putStr $ "result       "
                               let s1 = force $ showPat $ liftPats $ map compilePat patstrlst
                               return s1
                       _ -> error $ "doit13: unexpected code " ++ show code
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit13 False t

-------------------------------------------------------------------------------

  -- This is an adaptation of doit10 to user-defined datatypes,
  -- for testing GNFDataP, which is finally within reach thanks
  -- to SOP!
  doit14 :: Bool -> [(Int,String)] -> IO ()
  doit14 _ [] = do
                   putStrLn "----------------------------------------------------"
                   return ()
  doit14 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
                  _ -> error $ "doit14: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                 then do
                  putStrLn "===================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "===================================================="
                 else do
                  putStrLn "----------------------------------------------------"
                putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                 then putStrLn $  "expected value           = " ++ expect
                 else return ()
--              putStrLn $ "pat = " ++ show pat
                putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                 then putStrLn $  "expected value           = " ++ expect
                 else return ()
                s1 <- catch
                        ( return $! force $! get $! forcep patstr exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr "actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit14 False t

-------------------------------------------------------------------------------

