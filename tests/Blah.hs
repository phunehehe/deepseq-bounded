
-------------------------------------------------------------------------------

-- Disclaimer: I've not used HUnit yet. I'm using it only
-- as a "hook" so can compile/test in less clock time...
-- (I forget the details of how/why.)
--
-- Obviously, this should use proper HUnit eventually.
-- And QuickCheck and SmallCheck, DEFINITELY!!!...
-- This is a great case for those; and shape-syb no less.
--
-- This file could be cleaned up a lot, but that's
-- not quite a priority at the moment...
--
-- The reason I shied away from making it real HUnit tests
-- is probably related to the fancy exception throwing/catching
-- and catching of all exceptions (including error), etc.
-- Which was aggravated by the nature of this project and
-- of its tests, which hit bottom as a matter of course...
--
-- Yeah; and looking at examples of QuickCheck and SmallCheck,
-- it's still not clear to me how to work bottoming-out
-- into my tests.
--   I guess I could catch the bottoms in my code, and return
-- some reserved value to indicate that it occurred?...

-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if OVERLOADED_STRINGS
#error Please build with OVERLOADED_STRINGS flag set False, in order to run the tests.
#endif

-------------------------------------------------------------------------------

#define FOCUS_TEST 0
#define TRY_THIS 1
#define USE_TRACE 0

-------------------------------------------------------------------------------

-- Later: This may work with some versions of GHC, but I'm quite
-- sure I've seen it not work with some recent version...
-- Unfortunately, this doesn't work -- all pragmas are processed,
-- regardless of CPP (so CPP must run after).
-- The fact the CPP itself can be set via a pragma complicates this.
-- It could be made an exception, and the LANGUAGE CPP pragma could
-- be parsed regardless of CPP context (as all pragmas currently are).
-- As it is, we need separate source modules, and to switch at
-- import statements (and in the .cabal file).
-- THIS IS A BUG.
-- So anyhow, this is only here as a reminder:
---  #if ! HASKELL98_FRAGMENT
---  {-# LANGUAGE ... #-}
---  #endif
  {-# LANGUAGE DeriveGeneric #-}
  {-# LANGUAGE DeriveDataTypeable #-}  -- to make BottomedOut
  {-# LANGUAGE BangPatterns #-}

  {-# LANGUAGE Rank2Types #-}  -- for SYB (testing rnfpDyn and friends)

-------------------------------------------------------------------------------

  -- SOP pragmas:

  -- Not necessarily needed to work with SOP, but I think it'll
  -- be helpful here [?] ...
  {-# LANGUAGE ScopedTypeVariables #-}
  {-  LANGUAGE AllowAmbiguousTypes #-}

-- Actually, CPP doesn't work with Haskell (GHC) pragmas last I checked...
#if USE_SOP
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ < 708
  {-# LANGUAGE GADTs #-}
#endif
  {-# LANGUAGE TemplateHaskell #-}
#endif

-------------------------------------------------------------------------------

  module Blah ( run_tests ) where

-------------------------------------------------------------------------------

--import Common
  import Bottom
  import Foo
  import FooG

--import Control.DeepSeq.Generics
  import Control.DeepSeq.Bounded
--import Control.DeepSeq.Bounded hiding ( F )
  import Control.DeepSeq.Bounded.Generic

#if 0
  import GHC.Generics
--import GHC.Generics ( Generic )
#endif

  import Data.Maybe

  import Control.Exception
--import Control.Monad ( guard )
  import Control.Monad ( replicateM )
  import Control.Monad ( when )
  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )

  import Data.Data ( Data )

  import Data.Generics.Aliases ( mkQ, extQ, GenericQ )

--import Util ( spoor )
  import Debug.Trace ( trace )
  import Control.DeepSeq

#if __GLASGOW_HASKELL__ >= 708
  import Data.Typeable ( Proxy(..) )
#endif

#if USE_SOP
  import Generics.SOP hiding ( Shape )
#endif

  import System.Random

  import Data.List ( intercalate )

  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

  -- For convenience in GHCi interactive sessions:
  cp = compilePat
--cp = compilePat
  ip = intersectPats
  isp = subPat

  -- For more compact test expressions:
-- XXX Refer to Seqable.hs for comments about JUST_ALIAS_GSEQABLE.
#if 1
#if JUST_ALIAS_GSEQABLE
  fI :: forall a. Generic a => a -> a
  fI = force_ Insulate
  fP :: forall a. Generic a => a -> a
  fP = force_ Propagate
#else
  fI :: forall a. NFDataN a => a -> a
  fI = force_ Insulate
  fP :: forall a. NFDataN a => a -> a
  fP = force_ Propagate
#endif
#else
  fI = force_ Insulate
  fP = force_ Propagate
#endif

  gI :: forall a. Generic a => a -> a
  gI = gforce_ Insulate
  gP :: forall a. Generic a => a -> a
  gP = gforce_ Propagate

-------------------------------------------------------------------------------

  run_tests :: IO Int
  run_tests = do

    putStr "\n"
    -- So can notice the start when scrollback, esp. in ghci reload/reruns.
--  replicateM 50 $ putStrLn "################################################################################"
    putStrLn "\nTesting."
    putStrLn "\nNote that  __ = undefined  (abbrev. occurring in what follows)."

    -- XXX NOTE XXX
    --
    -- Most of the comments are pre-new-grammar (most of the comments
    -- are actually from no later than September 2014).  Some have been
    -- revised in light of the new grammar; many have not...

#if ! FOCUS_TEST

    let shs s p =
         unsafePerformIO $ 
         catch
          ( return $! force $ "input:                       " ++ show p ++ "\n" ++ "expect: " ++ s ++ "\n   got: " ++ showPat p )
          ( \e -> do let err = show (e :: ErrorCall)
                     return err )

    let eas = emptyPatNodeAttrs
    let neas = eas { depth = 23 }
    let peas = eas { doSpark = True }
    let pneas = neas { doSpark = True }
    let teas = eas { doConstrainType = True, typeConstraints = ["Int"] }
    let nteas = teas { depth = 23 }
    let pteas = teas { doSpark = True }
    let pnteas = nteas { doSpark = True }
    let teas2 = eas { doConstrainType = True, typeConstraints = ["[Int]"] }
    let pteas2 = teas2 { doSpark = True }
    let ptneas2 = pteas2 { depth = 23 }
    let xxptneas2 = ptneas2 { doTrace = True, doPing = True }
    let xxpeas = eas { doSpark = True, doTrace = True, doPing = True }

    putStrLn "\nTesting showPat"
    putStrLn "---------------"

    putStrLn $ shs "*" $ Node (WW eas) []
    putStrLn $ shs "." $ Node (WI eas) []
    putStrLn $ shs "!" $ Node (WS eas) []
    putStrLn $ shs "()" $ Node (WR eas) []
    putStrLn $ shs "(*)" $ Node (WR eas) [Node (WW eas) []]
    putStrLn $ shs "(**)" $ Node (WR eas) [Node (WW eas) [],Node (WW eas) []]
    putStrLn $ shs "(!*)" $ Node (WR eas) [Node (WS eas) [],Node (WW eas) []]
    putStrLn $ shs "(.!)" $ Node (WR eas) [Node (WI eas) [],Node (WS eas) []]
    putStrLn $ shs "(.)" $ Node (WR eas) [Node (WI eas) []]
    putStrLn $ shs "((.))" $ Node (WR eas) [Node (WR eas) [Node (WI eas) []]]
    putStrLn $ shs "(*.(*.(*.)**.*).*)" $ Node (WR eas) [Node (WW eas) [],Node (WI eas) [],Node (WR eas) [Node (WW eas) [],Node (WI eas) [],Node (WR eas) [Node (WW eas) [],Node (WI eas) []],Node (WW eas) [],Node (WW eas) [],Node (WI eas) [],Node (WW eas) []],Node (WI eas) [],Node (WW eas) []]
    putStrLn $ shs "(.!(*23*))" $ Node (WR eas) [Node (WI eas) [],Node (WS eas) [],Node (WR eas) [Node (WN neas) [],Node (WW eas) []]]
#if USE_PAR_PATNODE
    putStrLn $ shs "(.=(=*23*)*)" $ Node (WR eas) [Node (WI eas) [],Node (WR peas) [Node (WN pneas) [],Node (WW eas) []],Node (WW eas) []]
#endif
    putStrLn $ shs ":Int:()"    $ Node (TR teas)  []
--- putStrLn $ shs ":Int:!"     $ Node (TS teas)  []
    putStrLn $ shs ":Int:*23"   $ Node (TN nteas) []
    putStrLn $ shs ":Int:*"     $ Node (TW teas)  []
    putStrLn $ shs ":Int:."     $ Node (TI teas)  []
    putStrLn $ shs "=:Int:()"    $ Node (TR pteas)  []
--- putStrLn $ shs "=:Int:!"     $ Node (TS pteas)  []
    putStrLn $ shs "=:Int:*23"   $ Node (TN pnteas) []
    putStrLn $ shs "=:Int:*"     $ Node (TW pteas)  []
    putStrLn $ shs "=:Int:."     $ Node (TI pteas)  []
    putStrLn $ shs "=:[Int]:()"   $ Node (TR pteas2)  []
    putStrLn $ shs "=:[Int]:*"   $ Node (TW pteas2)  []
    putStrLn $ shs "=:[Int]:*23"   $ Node (TN ptneas2)  []
    putStrLn $ shs "=+^()"        $ Node (WR xxpeas)  []  -- canonical ordering
--  putStrLn $ shs "+^=()"        $ Node (WR xxpeas)  []
    putStrLn $ shs "=+^:[Int]:*23"   $ Node (TN xxptneas2)  []

--  let shp s = s ++ replicate (30 - length s) ' ' ++ show (compilePat s)
    let shp s = 
         unsafePerformIO $ do
           putStr $ s ++ replicate (30 - length s) ' '
           catch
            ( return $! force $ show (compilePat s) )
--          ( return $! force $ s ++ replicate (30 - length s) ' ' ++ show (compilePat s) )
            ( \e -> do let err = show (e :: ErrorCall)
                       return err )

    putStrLn "\nPattern                       Compiles to"
    putStrLn "-----------------------------------------"

--  putStrLn $ shp ""  -- empty pattern syntax error
    putStrLn $ shp "."  -- Node WI []  -- no parent to absorb (should be error)
    putStrLn $ shp "!"  -- Node WS []
    putStrLn $ shp "*"  -- Node WW []
    putStrLn $ shp "*!"  -- "disconnected pattern (not rooted)"
    putStrLn $ shp "()"  -- Node WR []
    putStrLn $ shp "(.)"  -- Node WR [Node WW []]
    putStrLn $ shp "(*)"  -- Node WR [Node WW []]
    putStrLn $ shp "(**)"  -- Node WR [Node WW [],Node WW []]
    putStrLn $ shp "(!*)"  -- Node WR [Node WS [],Node WW []]
    putStrLn $ shp "(.!)"  -- Node WR [Node WI [],Node WS []]
    putStrLn $ shp "(.(.))"  -- Node WR [Node WI [],Node WR [Node WI []]]
    putStrLn $ shp "((.))"  -- Node WR [Node WR [Node WI []]]
    putStrLn $ shp "(.(().))"  -- Node WR [Node WI [],Node WR [Node WR [],Node WI []]]
    putStrLn $ shp "(*.*.*.**.*.*)"
    putStrLn $ shp "(*.(*.(*.)*)*(.*).*)"
    putStrLn $ shp "(*.(*.(*.)*)*.*)"
    putStrLn $ shp "(*.(*.(*.)*)(*)(.*).*)"
    putStrLn $ shp "((!(!)(((!).!(!))))!(!(!)))"  -- of leaky fame
    putStrLn $ shp "*23"
    putStrLn $ shp "(.!(*23*))"
    putStrLn $ shp ":Int:"  -- see what happens
    putStrLn $ shp ":Int:()"
    putStrLn $ shp ":Int:!"  -- see what happens
    putStrLn $ shp ":Int."  -- see what happens
    putStrLn $ shp ":Int:."
    putStrLn $ shp ":Int:*"
    putStrLn $ shp ":Int*23"  -- see what happens
    putStrLn $ shp ":Int:*23"
#if USE_PAR_PATNODE
    putStrLn $ shp "=."  -- Node WI [] (doSpark=True)
    putStrLn $ shp "=!"  -- Node WS [] (doSpark=True)
    putStrLn $ shp "=*"  -- Node WW [] (doSpark=True)
    putStrLn $ shp "(.=(=*23*)*)"
    putStrLn $ shp "=:Int"  -- see what happens
    putStrLn $ shp "=:Int:()"
    putStrLn $ shp "=:Int:!"  -- see what happens
    putStrLn $ shp "=:Int:."
    putStrLn $ shp "=:Int:*"
    putStrLn $ shp "=:Int*23"  -- see what happens
    putStrLn $ shp "=:Int:*23"
    putStrLn $ shp "=:[Int]:()"
    putStrLn $ shp "=:[Int]:*"
    putStrLn $ shp "=:[Int]:*23"
    putStrLn $ shp "=+^()"
    putStrLn $ shp "+^=()"
    putStrLn $ shp "=+^:[Int]:*23"
#endif
    putStrLn $ shp " ! :G; How:. : A ()$)%&()(*&)*(_)+\"}>>=?}{\\:|B ; C ; D E F : ( . . ) "

    putStrLn ""
#endif

--  error "DEVEXIT"

#if ! FOCUS_TEST

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
           -- The next four tests give instance errors for [a].
           -- (Later: Silent pattern-match failures, that is, because
           -- WARN_PATTERN_MATCH_FAILURE is False by default.)
           -- But if testing expTG_* (GNFDataP/grnfp) then
           -- it's no longer a list...
         , (13, "((*.*).)")
         , (13, "((*...).)")
         , (13, "((*....).)")
         , (13, "((*....)!)")  -- used to have "23" but that's incorrect

         , (1,  "((*.)*)")  -- used to have "2" but that's incorrect
         , (1,  "((*.).)")
         , (1,  "((*(.*)).)")
         , (1,  "((*(*.)).)")  -- used to have "2" but that's incorrect
         , (13, "((((.*))).)")  -- See above comment about [a] instance
         , (1,  "(.(((.*))).)")  -- used to have "2" but that's incorrect

           -- (See Book, p.84.)
         , (1,  "(!(!(.(!!))).)")
         , (1,  "(!(!(.*)).)")
         , (1,  "(*(*(.*)).)")
         , (2,  "(..*)")
         , (2,  "(*.*)")
         , (1,  ":Tuple3:(*(*(.*)).)")  -- oops...
         , (1,  ":(,,):(*(*(.*)).)")
         , (2,  ":(,,):(*(*(!*)).)")
         , (1,  ":Bool:(***)")  -- XXX makes no sense (but works) -- oh well;
                                -- this is an important point to note in
                                -- docs/blog also... [Later: old comment;
                                -- syntax has changed a lot since written...]
         , (4,  ":Bool*")  -- used to be 1, but now it's a syntax error
         , (1,  ":Bool:*")  -- new syntax requires terminating empty {}
         , (2,  ":(,,):*")
         , (2,  "(*.!)")

-- XXX OLD COMMENT BUT BEWARE... XXX
--   "This does what-- it says, it will only match a childless Bool node.
--    Still need to implement # when not in type."
-- Since {} are obligatory in the new grammar, for type-constrained * and #,
-- better make sure the semantics are implemented as specified. (They are
-- specified in the Pattern module Haddock API page.)

           -- New comment: Since #::typename can only choose
           -- between effectively behaving as either WI or WS,
           -- it is never recursive:  It forces at most one node!
         , (1,  "(*:Bool:..)")  -- was 2 which is incorrect
         , (1,  "(*:Int:..)")  -- was 2 which is incorrect
         , (1,  "(*:(,):..)")  -- was 2 which is incorrect
         , (1,  "(*:OutOfScopeType:..)")  -- was 2 which is incorrect
         , (1,  "(*:[Int]:..)")
         , (2,  "(*:[Int]:.!)")
         , (1,  "(*.:Bool:.)")
         , (2,  "(*.:Int:.)")
         , (2,  "(*.:(,):.)")
         , (2,  "(*.:OutOfScopeType:.)")
         , (2,  "(*.:[Int]:.)")

         , (2,  "(*:[Int]:*.)")
         , (1,  "(*:Bool:*.)")
         , (1,  "(*:Int:*.)")
         , (1,  "(*:(,):*.)")
         , (1,  "(*:OutOfScopeType:*.)")
         , (2,  "(*.:Bool:*)")
         , (1,  "(*.:Int:*)")
         , (1,  "(*.:(,):*)")

         , (1,  "(**2.)")
         , (2,  "(**3.)")
         , (1,  "(*:G2:..)")
         , (1,  "(*:Int:..)")
         ]
    doit14 True test_patterns10
--  doit10 True test_patterns10
#endif

#if USE_SOP

-- Seems fine; it's not a good test as the # in the middle
-- only forces one ctor when type doesn't match, and need
-- to force one more level to hit an undefined.
#if 1
    let test_patterns11 = [
           (4,  "")  -- syntax error

         , (1,  "(*:G2:..)")
         , (1,  "(*:Int:..)")
         ]
    doit15 True test_patterns11
#endif

#endif

#if USE_SOP

    -- exp = G2 5 __ 7 :: TG
-- All fine last checked.
#if 1
    let test_patterns12 = [

--         (4, "..")

           (2, "*")
         , (1, "*0")
         , (1, "*1")
         , (2, "*2")
         , (2, "*3")

         , (1, "(!.!)")
         , (2, "(!!!)")
         , (1, "(!!)")  -- due to patmatch failure
         , (2, "(!!!)")
         , (1, "(!!!!)")  -- due to patmatch failure

         , (1, ":G1:(!!)")
         , (1, ":G1:(!!!)")
         , (1, ":G2:(!!)")
         , (2, ":G2:(!!!)")
         , (1, ":TG:(!!!)")

         , (1, ":G1:*")
         , (2, ":G2:*")
         , (1, ":TG:*")
         , (2, ":Int:.")
         , (2, ":G1:.")
         , (1, ":G2:.")
         , (2, ":TG:.")
         , (1, ":Int:.")

         , (1, ":G1:.(!!)")  -- (used to be a syntax err)
         , (1, ":G1:*(!!)")  -- (used to be a syntax err)

         ]
    doit16 True test_patterns12

--  error "DEVEXIT"
#endif

#endif

#if USE_SOP

-- This should work, once the patterns are right for lists,
-- but until I get non-list generic tests working I'm not
-- going to struggle with this.
#if 0
    let test_patterns13 = [
-- expTH_1 = H2 1 [H1 2.3, H3, H4 (H3, I3 I2 (H1 4.5))] False    (for ref.)
-- expTH_4 = H2 1 [H1 2.3, H3, H4 (__, I3 __ (H1 4.5))] __       (in use)
           (4,  "")  -- syntax error
         , (1,  "(!(!(!((.(.!))!))).)")
         , (2,  "(!(!(!((!(.!))!))).)")
         , (2,  "(!(!(!((.(!!))!))).)")
         , (2,  "(!(!(!((.(.!))!)))!)")
         , (1,  "(!((!)(!((.(.(!)))!))).)")
         , (2,  "(!((!)(!((!(.(!)))!))).)")
         , (2,  "(!((!)(!((.(!(!)))!))).)")
         , (2,  "(!((!)(!((.(.(!)))!)))!)")
         , (2,  "(!((!)(!((.(.!(!)))!)))!)")
         ]
    doit17 True test_patterns13
#endif

#if 1
    let test_patterns14 = [
-- expTJ_1 = J2 ( 1, J4 ( J3, K3 K2 ( J1 4.5))) False     -- for ref.
-- expTJ_2 = J2 ( 1, J4 ( J3, K3 __ ( J1 4.5))) False
-- expTJ_3 = J2 ( 1, J4 ( __, K3 K2 ( J1 4.5))) False
-- expTJ_4 = J2 ( 1, J4 ( __, K3 __ ( J1 4.5))) __        -- in use
--           {  { {} {  { {}  {  {} { {  {} }}}}}}}
-- XXX The tuples are ctors, I forgot...
           (4 ,  "")  -- syntax error
#if 0
         , (1 ,  "!")
         , (13,  "()")
         , (13,  "(!)")
         , (2 ,  "(!!)")
         , (13,  "(!!!)")
         , (13,  "(!!!!)")
         , (13,  "(!!!!!)")
#endif
-- expTJ_1 = J2 ( 1, J4 ( J3, K3 K2 ( J1 4.5))) False     -- for ref.
--                J2 { (,) { I J4 { (,) { J3 K3 { K2 { J1 { 4.5 } } } } } } False }
--                .  {  .  { .  . {  .  { .  .  {  . { .  { .   } } } } } } . }
           -- this pattern also double-checked on paper
         , (2 ,  "((!((!(!(!)))))!)")
--       , (1 ,  "((!((!(.(!))))).)")  -- blows (in partic. expTJ_2)
         , (1 ,  "((!((.(!(!))))).)")  -- blows (in partic. expTJ_3)
--       , (1 ,  "((!((.(.(!))))).)")  -- blows (in partic. expTJ_4)
#if 0
         , (1 ,  "((!(.(..(..(.))))).)")  -- = "((!(.)).)" (no blow)
         , (1 ,  "((!(.(.(.(!))))).)")  -- = "((!(.)).)" (no blow)
         , (1 ,  "((!(.(!(.(!))))).)")  -- = "((!(.)).)" (no blow)
         , (1 ,  "((!((!(.(!))))).)")  -- blows
         , (1 ,  "((!((..(..(.))))).)")  -- blows
         , (1 ,  "((!(.)).)")             -- confirming (no blow)
#endif
#if 0
         , (2 ,  "((!((!(((!))))))!)")
         , (1 ,  "((!((.(.((!)))))).)")
         , (1 ,  ".(.(..(.(..(.(.(.)))))).)")
         , (1 ,  "(.(..(.(..(.(.(.)))))).)")
         , (1 ,  "((..(.(..(.(.(.)))))).)")
         , (1 ,  "((!(.(..(.(.(.)))))).)")
#endif
#if 0
         , (1,  "(!(.(.(!))).)")
         , (2,  "(!(!(!(!)))!)")
         , (2,  "*")
         , (2,  "(!*!)")
         , (2,  "(***)")
#endif
         ]
    doit18 True test_patterns14
#endif

#if 1
    let test_patterns14b = [
-- expTJ_5 = J2 ( 1, J4 ( __, K2 )) False
           (4 ,  "")  -- syntax error
         , (2 ,  "((!((!!)))!)")
         , (1 ,  "((!((.!)))!)")
         , (1 ,  "((!(.(!!)))!)")
         , (1 ,  "((!.((!!)))!)")
         ]
    doit18b True test_patterns14b
#endif

#if 1
    let test_patterns14c = [
-- expTJ_6 = J4 ( __, K2 ) False
           (4 ,  "")  -- syntax error
         , (2 ,  "((!!))")
         , (1 ,  "((.!))")
         , (1 ,  "(.(!!))")
         , (1 ,  ".((!!))")
         ]
    doit18c True test_patterns14c
#endif

#if 1
    let test_patterns14d = [
-- expTJ_7 = ( __, K2 )
           (4 ,  "")  -- syntax error
         , (2 ,  "(!!)")
         , (1 ,  "(.!)")
         , (1 ,  ".(!!)")
         ]
    doit18d True test_patterns14d
#endif

#if 1
    let test_patterns14e = [
-- expTJ_8 = __ :: TJ
           (4 ,  "")  -- syntax error
         , (2 ,  "!")
         , (1 ,  ".")
         ]
    doit18e True test_patterns14e
#endif

#if 1
    let test_patterns14f = [
-- expTJ_8 = (K5 (__::TJ))
           (4 ,  "")  -- syntax error
         , (1 ,  "!")
         , (1 ,  "(.)")
         , (2 ,  "(!)")
         , (1 ,  ".")
         ]
    doit18f True test_patterns14f
#endif

-- These work!
#if 1
    let test_patterns14g = [
#if TRY_THIS
-- exp = K3 (__::TK) J3
#else
-- exp = K3 K2 (__::TJ)
#endif
           (4 ,  "")  -- syntax error
         , (1 ,  "!")
#if TRY_THIS
         , (1 ,  "(.!)")
         , (2 ,  "(!.)")
#else
         , (2 ,  "(.!)")
         , (1 ,  "(!.)")
#endif
         , (2 ,  "(!!)")
         , (1 ,  ".")
         ]
    doit18g True test_patterns14g
#endif

-- Okay last I checked.
#if 1
    let test_patterns15 = [
-- expTL_1 = L1 5.6 (M1 True)
-- expTL_2 = L1 5.6 (M1 __)
-- expTL_3 = L1 5.6 __
-- expTL_4 = L1 __ (M1 True)
           (4 ,  "")  -- syntax error
#if 0
#elif 1
         , (1 ,  ".")
         , (5 ,  ".(!!)")
         , (1 ,  "(!!)")
         , (2 ,  "((!)!)")
         , (1 ,  "((.)!)")
#elif 0
         , (1 ,  ".")
         , (5 ,  ".(!!)")
         , (2 ,  "(!!)")
         , (1 ,  "(.(!))")
#elif 0
         , (1 ,  ".")
         , (5 ,  ".(!!)")
         , (2 ,  "(!!)")
         , (2 ,  "(!(!))")
         , (2 ,  "(!(.))")
#elif 0
         , (1 ,  ".")
         , (5 ,  ".(!!)")
         , (1 ,  "(!!)")
         , (2 ,  "(!(!))")
         , (1 ,  "(!(.))")
#endif
         ]
    doit19 True test_patterns15
#endif

-- Good test case for the problem. No tuples; every step is generic.
#if 1
    let test_patterns16 = [
-- expTK_1 = K3 K2 ( J1 4.5)     -- for ref.
-- expTK_2 = K3 __ ( J1 4.5)
--           {  {} { {  {} }}}
-- XXX Don't forget tuples are ctors...
           (4 ,  "")  -- syntax error
#if 0
         , (2 ,  "(!!)")
         , (1 ,  "(.!)")
         , (2 ,  "(!.)")
         , (1 ,  "(..)")
#else
         , (2 ,  "(!(!))")  -- ok
         , (1 ,  "(..)")     -- X
         , (1 ,  "(.!)")     -- X
         , (1 ,  "(.(.))")  -- X
         , (1 ,  "(.(!))")  -- X
         , (2 ,  "(!.(!))")  -- X  & this does give the . with subpat. warning!
         , (2 ,  "(!.(.))")  -- X  & why no warning about . with subpattern?
         , (1 ,  "(..(.))")  -- X  & why no warning about . with subpattern?
         , (1 ,  ".(..(.))")  -- ok & why no warning about . with subpattern?
#endif
         ]
    doit20 True test_patterns16
#endif

#endif

---------------------------------------------

#endif

#if 0 || ! FOCUS_TEST

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

#endif

#if ! FOCUS_TEST

#if 1
    -- Testing splicePats
    let test_patterns12a = [
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
         , (1, "(:(,,):(...)(().).)", [1,0], [(0,"*"),(0,".")])
         ]
    doit12 True test_patterns12a
#endif

#if 1
    -- Testing elidePats
    -- XXX There's a nice way to do this, by rewriting each test
    -- automatically (or via manual regex substitutions) so that
    -- the target is the result of the corresponding splicePats test.
    -- Unfortunately, the expected results are not encoded for
    -- the splicePats test, so oh well, for now at least, just
    -- build some fresh tests!
    let test_patterns12b = [
           (1, "((!!)!(!))", [], [0])
         , (1, "((!!)!(!))", [0], [0])
         , (1, "((!!)!(!))", [1], [0])
         , (1, "((!!)!(!))", [1], [1])
         ]
    doit12b True test_patterns12b
#endif

#endif

#if ! FOCUS_TEST

#if 1
    -- Testing erodePat
    -- XXX Note we toss the stdgen when done here...
    stdgen12c <- newStdGen
--  let stdgen12c = mkStdGen seed
    let test_patterns12c = [
           (1, "((!!)!(!))", [])  -- recursive test...
         ]
--  stdgen12c <- doit12c True test_patterns12c
#if 0
    stdgen12c' <- doit12c True test_patterns12c stdgen12c
    stdgen12c'' <- doit12c True test_patterns12c stdgen12c'
    stdgen12c''' <- doit12c True test_patterns12c stdgen12c''
    return ()
#else
#if 1
    stdgen12c' <- doit12c True test_patterns12c stdgen12c 5
    return ()
#else
    doit12c True test_patterns12c stdgen12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
      >>= doit12c True test_patterns12c
#endif
#endif
#endif

#endif

#if 0 || ! FOCUS_TEST

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
--  --  4    unionPatssStr   :: [ String ] -> String
    --  5    emptyPat        :: Pat
    --  6    mkPat           :: forall d. Data d => d -> Pat
    --  7    growPat         :: forall d. Data d => Pat -> d -> Pat
    --  8    shrinkPat       :: Pat -> Pat
    --  9    liftPats        :: [ Pat ] -> Pat
    -- 10    splicePats      :: Pat -> [Int] -> [(Int, Pat)] -> Pat
    -- 11    isPath          :: Pat -> Bool
    -- 12    mkPatN          :: Int -> Pat -> Bool
    -- 13    elidePats       :: Pat -> Pat -> [Int] -> Pat
    -- 14    erodePat        :: StdGen -> [Int] -> Pat -> (Pat, StdGen)
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
         , ( 6, ["(!!!)", "(!.)"], "")
         -- matching against val = ([1,2,3::Int],(False,"foo"))
         -- mkPat val = "((!(!(!!)))(!(!(!(!!)))))"
         , ( 7, ["((!(!!))(!(!!)))"], "")
         , ( 7, ["((!(!(!!)))(!(!(!!))))"], "")
         , ( 7, ["((!(!(!!)))(!(!(!(!!)))))"], "")
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
         , (12, ["(!!!)", "(!.)"], "")
    -- 12    mkPatN          :: Int -> Pat -> Bool
    -- 13    elidePats        :: Pat -> Pat -> [Int] -> Pat
    -- 14    erodePat        :: StdGen -> [Int] -> Pat -> (Pat, StdGen)
         ]
    putStrLn "==================================================="
    putStrLn "Testing miscellaneous PatUtil functions..."
    doit13 True test_patterns13
#endif

#endif

#if ! FOCUS_TEST

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

#if 1
    putStr hdline
    putStrLn "Testing generic GNFDataN (refer to Foo.hs for the defs).\n"
    putStr hline
#if 0
#if ! USE_SOP
    putStrLn $ show $ from expTE_1
    putStrLn $ show $ from $ F 23
    putStrLn $ show $ from $ Just 11  -- bad comparison: two constructors!
#endif
--  putStrLn $ show $ from $ Just (undefined::Int)
--  putStrLn $ show $ from expTE_2
--  putStrLn $ show $ from expTE_3
    putStr "\n"
    putStrLn $ ""
           ++ "expBase7 = B2 (A1 True 4) (B1 True (A2 undefined))\n"
           ++ "expBase8 = B2 (A1 undefined 4) (B1 True (A2 undefined))\n"
           ++ "expBase9 = B2 (A1 undefined 4) (B1 undefined (A2 undefined))\n"
           ++ "...\n"
           ++ "getB_1 (B2 _ (B1 b _)) = b\n"
           ++ "getB_2 (B2 (A1 _ n) _) = n\n"
#endif
    s2 <- doit2 1 0 1 ""
    putStrLn s2
#if 0
    putStrLn $ ""
           ++ "expTB_15 = (A3 (B3 False undefined 5) False)\n"
           ++ "getA (A3 _ b) = show b\n"
    s3 <- doit3 1 1 1 ""
    putStrLn s3
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

#if USE_SOP

#if 1
    -- Testing rnfpDyn -- working
    putStrLn "\nTesting SOP rnfpDyn with SYB generic stop condition.\n"
#if 1
    let exp21 = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
    let get21 (_,xs,_) = (xs!!2)
    let f21_a :: [Int] -> PatNode
        f21_a _ = WI emptyPatNodeAttrs
    let f21_b :: Bool -> PatNode
        f21_b _ = WI emptyPatNodeAttrs
    let fg21 :: GenericQ PatNode
        fg21 = mkQ (WR emptyPatNodeAttrs) f21_a `extQ` f21_b
--  let fg21 = mkQ (WR emptyPatNodeAttrs) f21_a `extQ` f21_b :: forall a. Data a => a -> PatNode
--  let fg21 = mkQ (WR emptyPatNodeAttrs) f21_a `extQ` f21_b :: GenericQ PatNode
    putStrLn $ show $ get21 $ forcepDyn fg21 exp21
#else
    let exp21 = (3.4, [5,__,7], False) :: (Float, [Int], Bool)
    let get21 (_,xs,_) = (xs!!2)
#if 0
#elif 1
    let f21 :: [Int] -> PatNode
        f21 _ = WI emptyPatNodeAttrs
    let fg21 :: GenericQ PatNode
        fg21 = mkQ (WR emptyPatNodeAttrs) f21
#elif 0
    let f21 :: (Float,[Int],Bool) -> PatNode
    -- Cannot do in SYB? We can't just "stop on any 3-tuple"; we need
    -- to give the types of the components of the tuple as well?
--  let f21 :: (,,) a b c -> PatNode
--  let f21 :: (,,) * * * -> PatNode
--  let f21 :: (,,) -> PatNode
--  let f21 :: (a,b,c) -> PatNode
--  let f21 :: (Typeable a,Typeable b,Typeable c) => (a,b,c) -> PatNode
--  let f21 :: (Data a,Typeable a,Data b,Typeable b,Data c,Typeable c) => (a,b,c) -> PatNode
        f21 _ = WI emptyPatNodeAttrs
--      f21 (x,y,z) = WI emptyPatNodeAttrs
    let fg21 :: GenericQ PatNode
        fg21 = mkQ (WR emptyPatNodeAttrs) f21
#elif 0
    let f21 :: Int -> PatNode
        f21 x = WI emptyPatNodeAttrs
--      f21 x = case x of { 5 -> WR emptyPatNodeAttrs ; _ -> WI emptyPatNodeAttrs }  -- "case x" forces x!
--  let (f21::Int->PatNode) x = case x of { 5 -> WR emptyPatNodeAttrs ; _ -> WI emptyPatNodeAttrs }
    let fg21 :: GenericQ PatNode
        fg21 = mkQ (WR emptyPatNodeAttrs) f21
#endif
    putStrLn $ show $ get21 $ forcepDyn fg21 exp21
#endif
#endif

#if 1
    -- Testing rnfpDyn again -- not working...
    putStrLn "\nTesting SOP/SYB rnfpDyn again.\n"
#if 0
#elif 1
    let exp22 = [[[__]],[[__,6],[7]]] :: [[[Int]]]
-- These three should be exactly equivalent (same core produced)?
--  let get22 xs = (((xs!!1)!!1)!!0)
--  let get22 [_,[[_,n],_]] = n
    let get22 (_:((_:n:_):_):_) = n
    let f22a :: Int -> PatNode
        f22a _ = trace "Noo-A" $ WI emptyPatNodeAttrs
    let f22b :: [Int] -> PatNode
        f22b _ = trace "Noo-B" $ WI emptyPatNodeAttrs
    let f22c :: [[Int]] -> PatNode
        f22c _ = trace "Noo-C" $ WI emptyPatNodeAttrs
    let f22d :: [[[Int]]] -> PatNode  -- goes boom
        f22d _ = trace "Noo-D" $ WI emptyPatNodeAttrs
    let fg22 :: GenericQ PatNode
        fg22 = mkQ (WR emptyPatNodeAttrs) id
--                                                              -- blows
--                                                 `extQ` f22d  -- blows
                                       `extQ` f22c `extQ` f22d  -- FINE
--                         `extQ` f22b `extQ` f22c `extQ` f22d  -- FINE
--             `extQ` f22a `extQ` f22b `extQ` f22c              -- blows
--             `extQ` f22a `extQ` f22b `extQ` f22c `extQ` f22d  -- FINE
#elif 0
    let exp22 = [[[__]],[[__,6],__,[7]]] :: [[[Int]]]
    let get22 xs = (((xs!!1)!!2)!!0)
    let f22 :: Int -> PatNode        -- goes boom
--  let f22 :: [Int] -> PatNode      -- goes boom
--  let f22 :: [[Int]] -> PatNode    -- goes boom
--  let f22 :: [[[Int]]] -> PatNode  -- goes boom
        f22 _ = trace "Noo-A" $ WI emptyPatNodeAttrs
    let fg22 :: GenericQ PatNode
        fg22 = mkQ (WR emptyPatNodeAttrs) f22
#else
    let exp22 = (3.4, [[[__]],[[__,6],__,[7]]], __) :: (Float, [[[Int]]], Bool)
--  let exp22 = (3.4, [[__],[[__,6],__,[7]]], __) :: (Float, [[[Int]]], Bool)
    let get22 (_,xs,_) = (((xs!!1)!!2)!!0)
    let f22_a :: [[Int]] -> PatNode
        f22_a _ = WI emptyPatNodeAttrs
    let f22_b :: Bool -> PatNode
        f22_b _ = WI emptyPatNodeAttrs
    let fg22 :: GenericQ PatNode
        fg22 = mkQ (WR emptyPatNodeAttrs) f22_a `extQ` f22_b
--  let fg22 = mkQ (WR emptyPatNodeAttrs) f22_a `extQ` f22_b :: forall a. Data a => a -> PatNode
--  let fg22 = mkQ (WR emptyPatNodeAttrs) f22_a `extQ` f22_b :: GenericQ PatNode
#endif
    putStrLn $ show $ get22 $ forcepDyn fg22 exp22
#endif

#if 1
#if __GLASGOW_HASKELL__ >= 708
-- XXX This isn't working yet.
-- And it won't work naively either, b/c SOP is *shallow*...
-- Note:
-- *Foo> :m +Generics.SOP
-- *Foo Generics.SOP> datatypeInfo (Proxy :: Proxy [[Int]])
-- ADT "GHC.Types" "[]" (Constructor "[]" :* (Infix ":" RightAssociative 5 :* Nil))
-- So, as done in GNFDataP, we need to pattern-match in tandem on this,
-- through the *S and *P SOP recursion auxilliaries...
-- XXX I came back to this -- getting type errors, could probably
-- get rid of by adding extra functions (as per GNFDataP.hs code),
-- but remember, this is supposed to be API-user code ... if it's
-- going to be that painful to write, we have a problem...
-- (Still, can probably write friendlier wrapper functions.)
    -- Testing rnfpDyn' with SOP generic stop condition
    putStrLn "\nTesting SOP rnfpDyn' with SOP generic stop condition.\n"
#if 0
#elif 1
    putStrLn $ doit23
#elif 0
    let exp23 = [[[__]],[[__,6],__,[7]]] :: [[[Int]]]
    let get23 xs = (((xs!!1)!!2)!!0)
    let fg23 :: forall a. (Generic a, HasDatatypeInfo a) => a -> PatNode
        fg23 d | cname == "[[Int]]"  = WI emptyPatNodeAttrs
               | otherwise           = WR emptyPatNodeAttrs
          where
#if 1
           ADT mname tname (Constructor cname :* _) = dti
--         ADT mname tname (Constructor cname :* _) = dti :: DatatypeInfo (Code a)
--         (mname,tname,cname) = case dti of ...
--         ADT mname tname (Constructor cname :* (Infix ":" RightAssociative 5 :* Nil)) = dti
#endif
           dti = datatypeInfo proxy_a
           proxy_a = Proxy :: Proxy a
           xrep = from d
    putStrLn $ show $ get23 $ forcepDyn' fg23 exp23
#endif
#endif
#endif

#if 1
-- Gave up on the fully-SOP one, at least for now, b/c
-- it seems like it's hard to write (much uglier than SYB one),
-- and then writing a friendly wrapper looks tough as well.
-- But here is a 3rd alternative, hybrid SOP/Typeable which
-- is even simpler from the user perspective, only less efficient.
    putStrLn "Testing SOP rnfpDyn'' with Typeable generic stop condition\n"
    let exp24 = [[[__]],[[__,6],__,[7]]] :: [[[Int]]]
    let get24 xs = (((xs!!1)!!2)!!0)
    let fg24 :: forall a. Typeable a => a -> PatNode
        fg24 d | t == "[[[Int]]]"  = WI emptyPatNodeAttrs
               | t == "[[Int]]"    = WI emptyPatNodeAttrs
               | otherwise         = WR emptyPatNodeAttrs
         where t = show $ typeOf d
    -- This is a simple alternative to the SOP/SYB hybrid.
    -- If efficiency isn't prime concern, this would appear to be
    -- even simpler ... the above looks much more lucid to me than
    -- the usual SYB way of mkQ/extQ; here, just define one normal
    -- function, and away you go...
    putStrLn $ show $ get24 $ forcepDyn'' fg24 exp24
#endif

#endif

#endif

#if ! FOCUS_TEST

#if 1
    putStrLn "\nTesting Seqable class (tuple with list):\n"
    let test_patterns24 = [
           (1,  1, (3.4, [5, __, 7], False))                        -- okay
         , (2,  2, fP (3.4, fP (5 : fP (__ : fP (7 : []))), __))    -- okay
         , (2,  3, fP (3.4, fP (5 : fI (__ : fP (7 : []))), __))    -- okay
         , (1,  4, fI (3.4, fP (5 : fI (__ : fP (7 : []))), __))    -- XXX
         , (1,  5, fI (3.4, fI (5 : fI (__ : fP (7 : []))), __))    -- okay
         , (2,  6, fI (3.4, fI (5 : fP (__ : fP (7 : []))), __))    -- okay
         , (1,  7, fI (3.4, fP (5 : fI (__ : fP (7 : []))), True))  -- XXX
         , (1,  8, fP (3.4, fI (5 : fI (__ : fP (7 : []))), True))  -- XXX
         , (1,  9, fP (3.4, fI (5 : fI (__ : fI (7 : []))), True))  -- XXX
         ] :: [(Int,Int,(Float,[Int],Bool))]
    doit24 True test_patterns24
#endif

#if 1
    putStrLn "\nTesting Seqable class (nested tuples):\n"
    let test_patterns25 = [
           (1,  1, (3.4, (__, 7), False))       -- okay
         , (1,  2, fI (3.4 ,fI (__, 7) ,__))    -- okay
         , (2,  3, fP (3.4 ,fP (__, 7) ,__))    -- XXX
         , (2,  4, fP (3.4 ,fI (__, 7) ,__))    -- XXX
         , (2,  5, fI (3.4 ,fP (__, 7) ,__))    -- XXX
         , (1,  6, fI (3.4 ,fI (__, 7) ,True))  -- okay
         , (2,  7, fP (3.4 ,fP (__, 7) ,True))  -- XXX
         , (1,  8, fP (3.4 ,fI (__, 7) ,True))  -- okay
         , (2,  9, fI (3.4 ,fP (__, 7) ,True))  -- XXX
         ] :: [(Int,Int,(Float,(Bool,Int),Bool))]
    doit25 True test_patterns25
#endif

#if USE_SOP

#if 1
    putStrLn "\nTesting generic Seqable (tuple with list):\n"
    let test_patterns26 = [
           (1,  1, (3.4, [5, __, 7], False))                        -- okay
         , (2,  2, gP (3.4, gP (5 : gP (__ : gP (7 : []))), __))    -- okay
         , (2,  3, gP (3.4, gP (5 : gI (__ : gP (7 : []))), __))    -- okay
         , (1,  4, gI (3.4, gP (5 : gI (__ : gP (7 : []))), __))    -- XXX
         , (1,  5, gI (3.4, gI (5 : gI (__ : gP (7 : []))), __))    -- okay
         , (2,  6, gI (3.4, gI (5 : gP (__ : gP (7 : []))), __))    -- okay
         , (1,  7, gI (3.4, gP (5 : gI (__ : gP (7 : []))), True))  -- XXX
         , (1,  8, gP (3.4, gI (5 : gI (__ : gP (7 : []))), True))  -- XXX
         , (1,  9, gP (3.4, gI (5 : gI (__ : gI (7 : []))), True))  -- XXX
         ] :: [(Int,Int,(Float,[Int],Bool))]
    doit26 True test_patterns26
#endif

#if 1
    putStrLn "\nTesting generic Seqable (expTJ_*):\n"
-- expTJ_1 = J2 ( 1, J4 ( J3, K3 K2 ( J1 4.5))) False     -- for ref.
-- expTJ_2 = J2 ( 1, J4 ( J3, K3 __ ( J1 4.5))) False
-- expTJ_3 = J2 ( 1, J4 ( __, K3 K2 ( J1 4.5))) False
-- expTJ_4 = J2 ( 1, J4 ( __, K3 __ ( J1 4.5))) __        -- in use
    let test_patterns27 = [
           (1,  1, gI (J2 (gI ( 1, gI (J4 (gI ( __, gI (K3 __ ( gI (J1 4.5)))))))) __))
         , (2,  2, gP (J2 (gP ( 1, gP (J4 (gP ( __, gP (K3 __ ( gP (J1 4.5)))))))) __))
         , (1,  3, gI (J2 (gP ( 1, gP (J4 (gI ( __, gI (K3 __ ( gP (J1 4.5)))))))) __))
         , (2,  4, gP (J2 (gP ( 1, gP (J4 (gI ( __, gI (K3 __ ( gP (J1 4.5)))))))) __))
         , (2,  5, gI (J2 (gP ( 1, gP (J4 (gP ( __, gI (K3 __ ( gP (J1 4.5)))))))) __))
         , (2,  6, gI (J2 (gP ( 1, gP (J4 (gI ( __, gP (K3 __ ( gP (J1 4.5)))))))) __))
         ] :: [(Int,Int,TJ)]
    doit27 True test_patterns27
#endif

#endif

#if 1
    putStrLn "\nTesting PatNodeAttrs (pattern node attributes):\n"
    -- exp = G2 5 __ 7 :: TG
    let test_patterns28 = [
#if 1
           (4,  "^^!")   -- syntax error
         , (4,  "^=^!")  -- syntax error
         , (2,  "=*")  -- XXX broken (at least in observability sense)
         , (2,  "+*")  -- fine (but see no trace message!)
         , (2,  "^*")  -- fine (but get no exception)
#if 1
         , (1,  "+^!")
         , (2,  "+^*")
         , (2,  "+^(.!.)")
         , (3,  "^+()")
         , (3,  "^+(!!)")
         , (2,  "^+(!!!)")
         , (3,  "^+(!!!!)")
#else
-- Later: This is now working -- too well, and the first / encountered
-- puts an end to the testing!
         , (2,  "/*")  -- fine (but no indication of termination)
         , (1,  "/+^!")
         , (2,  "/+^*")
         , (2,  "/+^(.!.)")
         , (3,  "^+/()")
         , (3,  "^+/(!!)")
         , (2,  "^+/(!!!)")
         , (3,  "^+/(!!!!)")
#endif
#else
           (4,  "^^!")   -- syntax error
         , (4,  "^=^!")  -- syntax error
         , (3,  "^=()")
         , (3,  "^=(!!)")
         , (2,  "^=(!!!)")
         , (3,  "^=(!!!!)")
--       , (1,  "/=+^!")
--       , (2,  "/=+^*")       -- XXX
--       , (2,  "/=+^(.!.)")   -- XXX
         , (2,  "*")
         , (2,  "(.!.)")
         , (2,  "=*")  -- XXX broken (at least in observability sense)
         , (2,  "+*")  -- fine (but see no trace message!)
         , (2,  "^*")  -- fine (but get no exception)
--       , (2,  "/*")  -- fine (but no indication of termination)
--       , (2,  "/+^(.!.)")  -- fine
#endif
         ]
    doit28 True test_patterns28
#endif

#if 1
    putStrLn "\nTesting PatNodeAttrs (pattern node attributes):\n"
    -- exp = ([0..999999],[1000000..1999999]),[2000000..3999999])
    let test_patterns29 = [
           (1,  "+( +>ba(+*+*)=+*)")
         , (1,  "+(=+>ba(+*+*) +*)")
--       , (1,  "(>ba(+*+*)+=*)")
         ]
    doit29 True test_patterns29
#endif

#endif

    putStrLn "\n"

    return 0

-------------------------------------------------------------------------------

  hline :: String
  hline = "---------------------------------------------------\n"
  hdline :: String
  hdline = "===================================================\n"

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

--------------------------------

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
                   putStrLn "--------------------------------------------------"
                   return ()
  doit10 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit10: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
                   putStrLn "--------------------------------------------------"
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
                  putStrLn "==================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ ( forcep patstr2 . forcep patstr1 ) exp"
--                putStrLn "? = get $ ( forcep ( unionPatsStr [ patstr1, patstr2 ] ) ) exp"
                  putStrLn "? = get $! ( forcep_ ( unionPats [ pat1, pat2 ] ) ) exp"
                  putStrLn "(Results were more interesting with a previous semantics...)."
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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

  doit12b :: Bool -> [(Int,String,[Int],[Int])] -> IO ()
  doit12b _ [] = do
                   return ()
  doit12b b ((code,patstr1,path,isibs'):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  _ -> error $ "doit12b: unexpected code " ++ show code
    s <- catch
           ( do
                let target = compilePat patstr1
                let isibs = isibs'
                if b
                then do
                  putStr hdline
                  putStrLn "Testing elidePats."
                  putStr hline
                else return ()
                putStrLn $ "target       " ++ patstr1
                putStrLn $ "path         " ++ show path
                putStrLn $ "isibs        " ++ show isibs'
                let s1 = force $ showPat $ elidePats target path isibs
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
    doit12b False t

-------------------------------------------------------------------------------

  -- This is the first recursive test [much later than "12" would suggest].
  doit12c :: Bool -> [(Int,String,[Int])] -> StdGen -> Int -> IO StdGen
--doit12c :: Bool -> [(Int,String,[Int],Int)] -> StdGen -> IO StdGen
--doit12c :: Bool -> [(Int,String,StdGen,[Int],Int)] -> IO StdGen
--doit12c :: Bool -> [(Int,String,StdGen,[Int],Int)] -> IO ()
  doit12c _ [] g _ = do
                   return g
--                 return ()
  doit12c b lst@((code,patstr1,path):t) g 0 = return g
  doit12c b lst@((code,patstr1,path):t) g nreps = do  -- but singleton expected?
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
                  _ -> error $ "doit12c: unexpected code " ++ show code
    (s, g') <- catch
--  s <- catch
           ( do
                let target = compilePat patstr1
                if b
                then do
                  putStr hdline
                  putStrLn "Testing erodePat."
                  putStr hline
                else return ()
                putStrLn $ "target       " ++ patstr1
                putStrLn $ "stdgen       " ++ show g
                putStrLn $ "path         " ++ show path
                let (rslt, g') = erodePat g path target
                let s1 = force $ showPat $ rslt
--              let s1 = force $ showPat $ erodePat g path target
                putStr "result       "
                return $! (s1, g')
--              return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return (err, g))
--                   return err)
    putStrLn s
    putStr hline
    doit12c False lst g' (-1+nreps)
--  doit12c False t g' (-1+nreps)

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
    --  6  * mkPat           :: forall d. Data d => d -> Pat
    --  7  * growPat         :: forall d. Data d => Pat -> d -> Pat
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
                               putStr hline
                               putStrLn "Testing      mkPat ([1,2,3],(False,\"foo\"))"
                               putStr $ "result       "
                               let s1 = force $ showPat $ mkPat ([1,2,3::Int],(False,"foo"))
                               return s1
                       7 -> do
                               let patA = patstrlst!!0
                               putStr hline
                               putStrLn "Testing      growPat patA ([1,2,3],(False,\"foo\"))"
                               putStrLn $ "patA         " ++ patA
                               putStr $ "result       "
                               let s1 = force $ showPat $ growPat (compilePat patA) ([1,2,3::Int],(False,"foo"))
                               return s1
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
    -- 12    mkPatN          :: Int -> Pat -> Bool
    -- 13    elidePats        :: Pat -> Pat -> [Int] -> Pat
    -- 14    erodePat        :: StdGen -> [Int] -> Pat -> (Pat, StdGen)
                       12 -> do
                               putStr hline
                               putStrLn "Testing      mkPatN 2 ([1,2,3],(False,\"foo\"))"
                               putStr $ "result       "
                               let s1 = force $ showPat $ mkPatN 2 ([1,2,3::Int],(False,"foo"))
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

  doit14 :: Bool -> [(Int,String)] -> IO ()
  doit14 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit14 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit14: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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

#if USE_SOP

  -- This is an adaptation of doit10 to user-defined datatypes,
  -- for testing GNFDataP, which is finally within reach thanks
  -- to SOP!
  doit15 :: Bool -> [(Int,String)] -> IO ()
  doit15 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit15 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit15: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTG_5  -- ( G1, G2 5 __ 7, __ )
--              let exp = expTG_4  -- ( G1, G2 5 6 7, __ )
--              let exp = expTG_3  -- ( G1, G2 __ __ 7, __ )
--              let exp = expTG_2  -- ( __, G2 __ __ 7, __ )
--              let exp = expTG_1  -- ( G1, G2 5 6 7, True )
--              let exp = (3.4, [5,__,7], __) :: (Float, [Int], Bool)
                let pat = compilePat patstr
                let get = getG
--              let get (_,xs,_) = show $ (xs!!2)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (G1, G2 5 __ 7, __) :: (TG, TG, Bool)"
                  putStrLn "get (_,(G2 _ _ n),_) = n"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit15 False t

-------------------------------------------------------------------------------

  doit16 :: Bool -> [(Int,String)] -> IO ()
  doit16 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit16 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "pattern-match failure"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit16: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTG_6  -- G2 5 __ 7
                let pat = compilePat patstr
                let get = getG'
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = G2 5 __ 7 :: TG"
                  putStrLn "get (G2 _ _ n) = n"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit16 False t

-------------------------------------------------------------------------------

  doit17 :: Bool -> [(Int,String)] -> IO ()
  doit17 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit17 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "4.5"
                  2 -> "Prelude.undefined"
                  3 -> "4.5 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "4.5 (but \"# with subpattern\" warning)"
                  6 -> "4.5 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit17: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = expTH_1  -- H2 1 [H1 2.3, H3, H4 (H3, I3 I2 (H1 4.5))] False
--              let exp = expTH_2  -- H2 1 [H1 2.3, H3, H4 (__, I3 I2 (H1 4.5))] False
--              let exp = expTH_3  -- H2 1 [H1 2.3, H3, H4 (__, I3 I2 (H1 4.5))] __
                let exp = expTH_4  -- H2 1 [H1 2.3, H3, H4 (__, I3 __ (H1 4.5))] __
                let pat = compilePat patstr
                let get = getH
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = H2 1 [H1 2.3, H3, H4 (__, I3 I2 (H1 4.5))] __ :: TH"
--                putStrLn "exp = H2 1 [H1 2.3, H3, H4 (H3, I3 I2 (H1 4.5))] False :: TH"
                  putStrLn "get = (H2 _ [_, _, H4 (_, I3 _ (H1 f))] _) = show f"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit17 False t

-------------------------------------------------------------------------------

  doit18 :: Bool -> [(Int,String)] -> IO ()
  doit18 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "4.5"
                  2 -> "Prelude.undefined"
                  3 -> "4.5 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "4.5 (but \"# with subpattern\" warning)"
                  6 -> "4.5 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = expTJ_1  -- J2 (1, J4 (J3, K3 K2 (J1 4.5))) False
--              let exp = expTJ_2  -- J2 (1, J4 (J3, K3 __ (J1 4.5))) False
                let exp = expTJ_3  -- J2 (1, J4 (__, K3 K2 (J1 4.5))) False
--              let exp = expTJ_4  -- J2 (1, J4 (__, K3 __ (J1 4.5))) __
                let pat = compilePat patstr
                let get = getJ
                if b
                then do
                  putStrLn "==================================================="
--                putStrLn "exp = J2 (1, J4 (J3, K3 K2 (J1 4.5))) False"
                  putStrLn "exp = J2 (1, J4 (J3, K3 __ (J1 4.5))) False"
--                putStrLn "exp = J2 (1, J4 (__, K3 K2 (J1 4.5))) False"
--                putStrLn "exp = J2 (1, J4 (__, K3 __ (J1 4.5))) __"
                  putStrLn "get = (J2 (_, J4 (_, K3 _ (J1 f))) _) = show f"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18 False t

-------------------------------------------------------------------------------

  doit18b :: Bool -> [(Int,String)] -> IO ()
  doit18b _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18b b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "1"
                  2 -> "Prelude.undefined"
                  3 -> "1 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "1 (but \"# with subpattern\" warning)"
                  6 -> "1 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18b: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTJ_5  -- J2 (1, J4 (__, K2)) False
                let pat = compilePat patstr
                let get = getJ'
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = J2 (1, J4 (__, K2)) False"
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
--                putStrLn "get (J2 (n, J4 (_, K2)) False) = show n"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18b False t

-------------------------------------------------------------------------------

  doit18c :: Bool -> [(Int,String)] -> IO ()
  doit18c _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18c b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "1"
                  2 -> "Prelude.undefined"
                  3 -> "1 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "1 (but \"# with subpattern\" warning)"
                  6 -> "1 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18c: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTJ_6  -- J4 (__, K2)
                let pat = compilePat patstr
                let get = getJ6
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = J4 (__, K2)"
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
--                putStrLn "get (J4 (_, K2)) = show n"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18c False t

-------------------------------------------------------------------------------

  doit18d :: Bool -> [(Int,String)] -> IO ()
  doit18d _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18d b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "1"
                  2 -> "Prelude.undefined"
                  3 -> "1 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "1 (but \"# with subpattern\" warning)"
                  6 -> "1 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18d: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTJ_7  -- (__, K2)
                let pat = compilePat patstr
                let get = getJ7
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (__, K2)"
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18d False t

-------------------------------------------------------------------------------

  doit18e :: Bool -> [(Int,String)] -> IO ()
  doit18e _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18e b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "1"
                  2 -> "Prelude.undefined"
                  3 -> "1 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "1 (but \"# with subpattern\" warning)"
                  6 -> "1 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18e: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTJ_8  -- __ :: TJ
                let pat = compilePat patstr
                let get = getJ8
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = __ :: TJ"
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18e False t

-------------------------------------------------------------------------------

  doit18f :: Bool -> [(Int,String)] -> IO ()
  doit18f _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18f b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "okay"
                  2 -> "Prelude.undefined"
                  3 -> "okay (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "okay (but \"# with subpattern\" warning)"
                  6 -> "okay (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18f: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (K5 (__::TJ))
                let pat = compilePat patstr
                let get _ = "okay"
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (K5 (__::TJ))"
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18f False t


-------------------------------------------------------------------------------

  doit18g :: Bool -> [(Int,String)] -> IO ()
  doit18g _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit18g b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "okay"
                  2 -> "Prelude.undefined"
                  3 -> "okay (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "okay (but \"# with subpattern\" warning)"
                  6 -> "okay (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit18g: unexpected code " ++ show code
    s <- catch
           ( do
#if TRY_THIS
                let exp = K3 (__::TK) J3
#else
                let exp = K3 K2 (__::TJ)
#endif
                let pat = compilePat patstr
                let get _ = "okay"
                if b
                then do
                  putStrLn "==================================================="
#if TRY_THIS
                  putStrLn "exp = K3 (__::TK) J3"
#else
                  putStrLn "exp = K3 K2 (__::TJ)"
#endif
                  putStrLn "get ~_ = show 1  -- seems to suffice?!?..."
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit18g False t

-------------------------------------------------------------------------------

  doit19 :: Bool -> [(Int,String)] -> IO ()
  doit19 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit19 b ((code,patstr):t) = do
--  let expstr = "True"
    let expstr = "5.6"
    let expect = case code of
                  1 -> expstr
                  2 -> "Prelude.undefined"
                  3 -> expstr ++ " (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> expstr ++ " (but \"# with subpattern\" warning)"
                  6 -> expstr ++ " (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit19: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = expTL_1  -- L1 5.6 (M1 True)
                let exp = expTL_2  -- L1 5.6 (M1 __)
--              let exp = expTL_3  -- L1 5.6 __
--              let exp = expTL_4  -- L1 __ (M1 True)
                let pat = compilePat patstr
                let get = getL
--              let get = getL'
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = L1 5.6 (M1 __)"
--                putStrLn "exp = L1 5.6 __"
--                putStrLn "exp = L1 __ (M1 True)"
                  putStrLn "get (L1 f (M1 _)) = show f"
--                putStrLn "get (L1 f _) = show f"
--                putStrLn "get (L1 _ (M1 b)) = show b"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit19 False t

-------------------------------------------------------------------------------

  doit20 :: Bool -> [(Int,String)] -> IO ()
  doit20 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit20 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "4.5"
                  2 -> "Prelude.undefined"
                  3 -> "4.5 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "4.5 (but \"# with subpattern\" warning)"
                  6 -> "4.5 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit20: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = expTK_1  -- K3 K2 (J1 4.5)
                let exp = expTK_2  -- K3 __ (J1 4.5)
                let pat = compilePat patstr
                let get = getK
                if b
                then do
                  putStrLn "==================================================="
--                putStrLn "exp = K3 K2 (J1 4.5)"
                  putStrLn "exp = K3 __ (J1 4.5)"
                  putStrLn "get = (K3 _ (J1 f)) = show f"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit20 False t

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
  doit23 :: String
  doit23 = show $ fg23 exp23
--doit23 = show $ get23 $ forcepDyn' fg23 exp23
   where
    exp23 = [[[__]],[[__,6],__,[7]]] :: [[[Int]]]
    get23 xs = (((xs!!1)!!2)!!0)
#if 0
    fg23 :: forall a. (Generic a, HasDatatypeInfo a) => a -> DatatypeInfo (Code a)
    fg23 d = dti
#else
    fg23 :: forall a. (Generic a, HasDatatypeInfo a, All2 Show (Code a), Typeable a) => a -> PatNode
#if 1
    fg23 d = doit23' dti proxy_a d (from d)
#else
    fg23 d | cname == "[[Int]]"  = WI emptyPatNodeAttrs
           | otherwise           = WR emptyPatNodeAttrs
#endif
#endif
     where
--    ADT mname tname (Constructor cname :* _) = dti
--    ADT mname tname (Constructor cname :* _) = dti :: DatatypeInfo (Code a)
--    (mname,tname,cname) = case dti of ...
--    ADT mname tname (Constructor cname :* (Infix ":" RightAssociative 5 :* Nil)) = dti
      dti = datatypeInfo proxy_a
      proxy_a = Proxy :: Proxy a
      xrep = from d

  doit23' :: forall a.
             (
               Generic a
             , HasDatatypeInfo a
--           , All2 NFDataP (Code a)
             , All2 Show (Code a)
             , Typeable a
--           , NFDataN a
--           , NFDataP a
             ) =>
                     DatatypeInfo (Code a)
                  -> Proxy a
                  -> a
                  -> Rep a
                  -> PatNode
  doit23' (ADT     _ _ cs) proxy_a x xrep
   = doit23'' cs         proxy_a x xrep
  doit23' (Newtype _ _ c ) proxy_a x xrep
   = doit23'' (c :* Nil) proxy_a x xrep

  doit23'' :: forall a xss.
            (
              Generic a
            , HasDatatypeInfo a
            , All2 Show xss
            ) =>
                    NP ConstructorInfo xss
                 -> Proxy a
                 -> a
                 -> SOP I xss
                 -> PatNode
  doit23'' (m :* _) proxy_a x (SOP (Z xs))
   | tx == "[[Int]]"  = WI emptyPatNodeAttrs
   | otherwise        = WR emptyPatNodeAttrs
   where
    !_ = trace ("*** "++tx) $ ()
    tx | (Constructor n) <- m  = n
       | (Infix n _ _) <- m    = n
       | (Record n _) <- m     = n
  doit23'' (m :* ms) proxy_a x (SOP (S xss))
   = doit23'' ms proxy_a x (SOP xss)
  doit23'' _ _ _ _ = error "doit23'': unexpected case!!"
#endif

-------------------------------------------------------------------------------

#endif

  doit24 :: Bool -> [(Int,Int,(Float,[Int],Bool))] -> IO ()
  doit24 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit24 b ((code,ident,exp):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "7 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit24: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = force_ Propagate (force_ Propagate 3.4, force_ Propagate (force_ Propagate 5 : (force_ Propagate __ : (force_ Propagate 7 : force_ Propagate []))), force_ Propagate __) :: (Float, [Int], Bool)
--              let seq_exp = mkSeqableHarness exp
--              let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = <refer to test code>"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $! exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
--              putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
--              putStrLn $ "pat = " ++ show pat
--              putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
                s1 <- catch
                        ( return $! force $! get $! exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr $ show ident ++ " actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit24 False t

-------------------------------------------------------------------------------

  doit25 :: Bool -> [(Int,Int,(Float,(Bool,Int),Bool))] -> IO ()
  doit25 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit25 b ((code,ident,exp):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "7 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit25: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = force_ Propagate (force_ Propagate 3.4, force_ Propagate (force_ Propagate 5 : (force_ Propagate __ : (force_ Propagate 7 : force_ Propagate []))), force_ Propagate __) :: (Float, [Int], Bool)
--              let seq_exp = mkSeqableHarness exp
--              let pat = compilePat patstr
                let get (_,(_,x),_) = show x
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = <refer to test code>"
                  putStrLn "get (_,(_,x),_) = show x"
                  putStrLn "? = get $! exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
--              putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
--              putStrLn $ "pat = " ++ show pat
--              putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
                s1 <- catch
                        ( return $! force $! get $! exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr $ show ident ++ " actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit25 False t

-------------------------------------------------------------------------------

#if USE_SOP

  doit26 :: Bool -> [(Int,Int,(Float,[Int],Bool))] -> IO ()
  doit26 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit26 b ((code,ident,exp):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "7 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit26: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = force_ Propagate (force_ Propagate 3.4, force_ Propagate (force_ Propagate 5 : (force_ Propagate __ : (force_ Propagate 7 : force_ Propagate []))), force_ Propagate __) :: (Float, [Int], Bool)
--              let seq_exp = mkSeqableHarness exp
--              let pat = compilePat patstr
                let get (_,xs,_) = show $ (xs!!2)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = <refer to test code>"
                  putStrLn "get (_,xs,_) = show $ (xs!!2)"
                  putStrLn "? = get $! exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
--              putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
--              putStrLn $ "pat = " ++ show pat
--              putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
                s1 <- catch
                        ( return $! force $! get $! exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr $ show ident ++ " actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit26 False t

-------------------------------------------------------------------------------

  doit27 :: Bool -> [(Int,Int,TJ)] -> IO ()
  doit27 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit27 b ((code,ident,exp):t) = do
    let expect = case code of
                  1 -> "4.5"
                  2 -> "Prelude.undefined"
                  3 -> "4.5 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "4.5 (but \"# with subpattern\" warning)"
                  6 -> "4.5 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit27: unexpected code " ++ show code
    s <- catch
           ( do
--              let exp = expTJ_1  -- J2 (1, J4 (J3, K3 K2 (J1 4.5))) False
--              let exp = expTJ_2  -- J2 (1, J4 (J3, K3 __ (J1 4.5))) False
--              let exp = expTJ_3  -- J2 (1, J4 (__, K3 K2 (J1 4.5))) False
--              let exp = expTJ_4  -- J2 (1, J4 (__, K3 __ (J1 4.5))) __
--              let pat = compilePat patstr
                let get = getJ
                if b
                then do
                  putStrLn "==================================================="
--                putStrLn "exp = J2 (1, J4 (J3, K3 K2 (J1 4.5))) False"
--                putStrLn "exp = J2 (1, J4 (J3, K3 __ (J1 4.5))) False"
--                putStrLn "exp = J2 (1, J4 (__, K3 K2 (J1 4.5))) False"
                  putStrLn "exp = J2 (1, J4 (__, K3 __ (J1 4.5))) __"
                  putStrLn "get = (J2 (_, J4 (_, K3 _ (J1 f))) _) = show f"
                  putStrLn "? = get exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
--              putStrLn $ "patstr                   = " ++ patstr
                if code > 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
--              putStrLn $ "pat = " ++ show pat
--              putStrLn $ "showPat.compilePat       = " ++ showPat pat
                if code <= 4
                then putStrLn $  show ident ++ " expected value           = " ++ expect
                else return ()
                s1 <- catch
                        ( return $! force $! get $! exp )
                        (\e -> do let err = show (e :: ErrorCall)
--                      (\e -> do let err = show (e :: BottomedOut)
                                  return err)
                putStr $ show ident ++ " actual value             = "
                return $! s1
           )
           -- ErrorCall if use __ = undefined
           -- BottomedOut if use __ = throw BottomedOut
           (\e -> do let err = show (e :: ErrorCall)
--         (\e -> do let err = show (e :: BottomedOut)
                     return err)
    putStrLn s
    doit27 False t

-------------------------------------------------------------------------------

  doit28 :: Bool -> [(Int,String)] -> IO ()
  doit28 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit28 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "7"
                  2 -> "Prelude.undefined"
                  3 -> "7 (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "7 (but \"# with subpattern\" warning)"
                  6 -> "7 (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "7 (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "7 (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit28: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = expTG_6  -- G2 5 __ 7
                let pat = compilePat patstr
                let get = getG'
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = G2 5 __ 7 :: TG"
                  putStrLn "get (G2 _ _ n) = n"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit28 False t

-------------------------------------------------------------------------------

  doit29 :: Bool -> [(Int,String)] -> IO ()
  doit29 _ [] = do
                   putStrLn "--------------------------------------------------"
                   return ()
  doit29 b ((code,patstr):t) = do
    let expect = case code of
                  1 -> "((999999,1999999),3999999)"
                  2 -> "Prelude.undefined"
                  3 -> "((999999,1999999),3999999) (due to pattern-match failure)"
                  4 -> "syntax error"
                  5 -> "((999999,1999999),3999999) (but \"# with subpattern\" warning)"
                  6 -> "((999999,1999999),3999999) (but \"constraint/subpattern arity mismatch\" warning)"
#if WARN_PATTERN_MATCH_FAILURE
                  13 -> "((999999,1999999),3999999) (but with pattern-match failure warning)"
                  23 -> "Prelude.undefined (but with pattern-match failure warning)"
#else
                  13 -> "((999999,1999999),3999999) (with silent pattern-match failure)"
                  23 -> "Prelude.undefined (with silent pattern-match failure)"
#endif
                  _ -> error $ "doit29: unexpected code " ++ show code
    s <- catch
           ( do
                let exp = (([0..999999::Int],[1000000..1999999::Int]),[2000000..3999999::Int])
                let pat = compilePat patstr
                let get ((xs,ys),zs) = show ((last xs,last ys),last zs)
                if b
                then do
                  putStrLn "==================================================="
                  putStrLn "exp = (([0..999999],[1000000..1999999]),[2000000..3999999])"
                  putStrLn "get ((xs,ys),zs) = show ((last xs,last ys),last zs)"
                  putStrLn "? = get $ forcep patstr exp"
                  putStrLn "==================================================="
                else do
                  putStrLn "---------------------------------------------------"
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
    doit29 False t

-------------------------------------------------------------------------------

#endif

