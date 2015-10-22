
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

-- XXX This is not correct! Use elidePats if you want something
-- arity-changing.  The only time it would be valid for shrinkPat
-- to produce a (Node WR []) is from some attribute-enriched
-- (Node WR []) or (Node TR []).
#define SHRINK_TO_EMPTY_WR 0

-- As for intersection, if the arities differ, the node
-- effectively becomes non-recursive.  (Whether this is
-- theoretically the best choice is still uncertain.)
-- What is certain is, unless support richer Pattern's,
-- a union of two recursive Pattern nodes with differing
-- arities is not well-definable.
--   The reason this switch exists at all is, it can
-- be expensive to compute this predicate, especially
-- considering that recursive nodes are very common,
-- with WR being ("in the average case") the single most
-- abundant node type in a pattern, probably followed
-- in order by WI, WS, WW, WN (or parallel counterparts).
#define ENFORCE_SAME_ARITY_UNION 1

#define DO_TRACE 0

-- Now specified via --flag=[-]USE_WWW_DEEPSEQ
--- #define USE_WW_DEEPSEQ 1

-------------------------------------------------------------------------------

  -- XXX For debugging only!
  {-# LANGUAGE BangPatterns #-}

  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE Rank2Types #-}
  {-  LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.PatUtil
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.PatUtil
  (

     -- * Basic operations on Patterns

       unionPats
     , intersectPats
     , subPat
--   , unionPatsStr

#if ! HASKELL98_FRAGMENT
     -- * Operations for obtaining and modifying Patterns based on a term

     , mkPat
     , mkPatN
     , growPat
#endif

     -- * Operations for obtaining subpatterns (in the 'subPat' sense)

     , truncatePat
     , shrinkPat

     -- * Operations for the direct construction and perturbation of Patterns

     , emptyPat
     , liftPats

     , splicePats
     , elidePats

     , erodePat

     -- * Re-exported for convenience

     , module Control.DeepSeq.Bounded.Pattern

     -- * Debugging convenience

#if ! HASKELL98_FRAGMENT
     , Shape
     , shapeOf
     , ghom
#endif
     , probDensRose
     , weightedRose
     , unzipRose
     , showRose

  )
  where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded.Pattern

  import Data.Maybe ( isNothing, fromJust )
  import Data.Maybe ( isJust )

#if ! HASKELL98_FRAGMENT
  import Data.Data ( Data )
  import Data.Generics ( GenericQ )
  import Data.Generics ( gmapQ )
#endif

  import Data.List ( findIndex )
--import Data.List ( elemIndex )
  import Data.List ( sortBy )
--import Data.List ( nub )
  import Data.List ( foldl' )
  import Data.List ( group )
  import Data.List ( sort )
  import Data.List ( intersect )

  import System.Random

  import Control.Concurrent ( ThreadId )

--import Control.Applicative ( (<$) )

  import Debug.Trace ( trace )
  import Control.DeepSeq ( force )

  -- For global Bool state, to indicate that a warning has already been issued.
  import Data.IORef
  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
#else
  mytrace _ = id
#endif

-------------------------------------------------------------------------------

  firstWarningPassed :: IORef Bool
  firstWarningPassed = unsafePerformIO $ newIORef False

-------------------------------------------------------------------------------

  -- | Compute the union of a list of 'Pattern's.
  --
  -- Note that unionPats is undefined when homologous nodes
  -- specify incompatible arities (only possible when
  -- 'WR' or 'TR' are involved).
  --
  -- /__XXX__ Support for the various attributes is work in progress. It may be impossible to arrive at a consistent treatment for all attributes under unions. This is work in progress, but the five un-modified @W*@ node types should be safe./
  unionPats :: [ Pattern ] -> Pattern
  unionPats [] = emptyPat
  -- Using force here, to try to prevent the regular output being
  -- interleaved with the trace message. it works, except for the
  -- opening " (if you're in ghci, anyways).
  unionPats ps = foldr1 (union' False) $ force $ map (clearTypeConstraints "unionPats") ps
--unionPats ps = foldr1 (union' False) ps
--unionPats ps = foldr1 union' $ trace (">> " ++ show ps) $ ps

  union' :: Bool -> Pattern -> Pattern -> Pattern

  -- We hit this coming in every time [modulo compiler/runtime
  -- optimisations], so performance counts.
  union' b (Node pas1 cs1) (Node pas2 cs2)
   | WI{} <- pas1               = Node pas2' cs2
   |               WI{} <- pas2 = Node pas1' cs1
   where
    pas1' = setPatNodeAttrs pas1 union_as
    pas2' = setPatNodeAttrs pas2 union_as
    union_as = unionAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)

  -- Symmetric cases:
  union' _ nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   -- XXX Those cases using zipWith are not correct,
   -- unless length cs1 == length cs2.  But one hates to
   -- have to compute that. Caveat Emptor! This will behave
   -- as a true union operator only if the child pattern lists
   -- are compatibly-sized!
   --   In seqaid (and in "correct" manual usage?), this problem
   -- will never arise.
#if 0
   -- XXX Don't do it! -- (==) on PatNode might be more expensive
   -- than I think, some attributes have list parameters...
   | p1 == p2           = Node p1 $ zipWith (union' b) cs1 cs2
#endif
#if ENFORCE_SAME_ARITY_UNION
   | WR{} <- pas1, WR{} <- pas2 = {-trace "WRWR" $-}
      if csokay then Node (WR union_as) $ zipWith (union' False) cs1 cs2
      else error "unionPat: WRWR: encountered arity disparity!"
#else
   | WR{} <- pas1, WR{} <- pas2 = {-trace "WRWR" $-} Node (WR union_as) $ zipWith (union' False) cs1 cs2
#endif
   | WS{} <- pas1, WS{} <- pas2 = {-trace "WSWS" $-} Node (WS union_as) []
   | WN{} <- pas1, WN{} <- pas2 = {-trace "WNWN" $-} Node (WN union_as) []
#if USE_WW_DEEPSEQ
   | WW{} <- pas1, WW{} <- pas2 = {-trace "WWWW" $-} Node (WW union_as) []
#endif
#if 0
#if ENFORCE_SAME_ARITY_UNION
   | TR{} <- pas1, TR{} <- pas2 = {-trace "TRTR" $-}
      if csokay then Node (TR union_as) $ zipWith (union' False) cs1 cs2
      else error "unionPat: TRTR: encountered arity disparity!"
#else
   | TR{} <- pas1, TR{} <- pas2
      = {-trace "TRTR" $-} Node (TR union_as) $ zipWith (union' False) cs1 cs2
#endif
   | TI{} <- pas1, TI{} <- pas2
      = {-trace "TITI" $-} Node (TI union_as) []
---   = {-trace "TITI" $-} Node (TI union_as) $ zipWith (union' False) cs1 cs2
----  = {-trace "TITI" $-} Node (TI union_as (intersectTys tys1 tys2)) $ zipWith (union' False) cs1 cs2
--- | TR{} cls1 tys1 cns1 <- p1, TR{} cls2 tys2 cns2 <- p2
----  = {-trace "TRWR" $-} Node (TR union_as (cls1++cls2) (tys1++tys2) (cns1++cns2)) $ zipWith union' cs1 cs2
#endif
   | otherwise  = union'asym_1 False (Node pas1 cs1) (Node pas2 cs2)
   where
    pas1' = setPatNodeAttrs pas1 union_as
    pas2' = setPatNodeAttrs pas2 union_as
    union_as = unionAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#if ENFORCE_SAME_ARITY_UNION
    csokay = equilength cs1 cs2
#endif

  -- Now the asymmetric cases:
  -- XXX This probably has numerous bugs and oversights!
   -- XXX Those cases using zipWith are not correct,
   -- unless length cs1 == length cs2.  But one hates to
   -- have to compute that. Caveat Emptor! This will behave
   -- as a true union operator only if the child pattern lists
   -- are compatibly-sized!
   --   Actually, if zipWith (which is acting on two [Pattern];
   -- you could supply union-unit pattern for missing children
   -- in shorter list, but then which children exactly were missing?
   -- So rather than play such a game without a racket, we just
   -- require the lists to be the same length or all bets are off.
   --   In seqaid, this problem will never arise.
   -- And in "correct" manual usage?...

#if USE_WW_DEEPSEQ
  union'asym_1 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WW{} <- pas1
      =
--      trace (if b then "**WW" else "WW**") $
        Node (WW union_as) []
   | not b      = union'asym_1 True nod2 nod1
   | otherwise  = union'asym_2 False nod1 nod2
   where
    union_as = unionAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#endif

  union'asym_2 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WN{} <- pas1
      =
--      trace (if b then "**WN" else "WN**") $
        union'asym_2_aux dep
   | not b      = union'asym_2 True nod2 nod1
   | otherwise  = union'asym_3 False nod1 nod2
   where
    union'asym_2_aux 0 = Node (WI union_as) []
    union'asym_2_aux 1 = Node (WS union_as) []
    union'asym_2_aux _ = Node (WN union_as) []
    union_as = unionAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
    dep = depth as1

-- XXX XXX
-- I think it is not possible to reconcile, say, WR with TW.
-- TW says rnf if in type; WR says recurse regardless of type
-- according to the subpatterns.  Ah! But the union is, a priori,
-- only well-defined if the WR shape is "elected" from the wild
-- shape of TW, so in fact the correct answer is TR!  Very interesting!!!
-----
-- But what of WR with TN?...  If WR pattern depth (levels of tree)
-- is less than the n of TN, then answer is again TR.  But if n is
-- deeper than the WR pattern shape?... Perhaps only in SOME places...
--   You can still perhaps reconcile it, but now some fairly serious
-- pattern surgery is required! "We have the technology"....
-------
  -- We have now to deal only with asymmetric combos
  -- of WR, TR, WS, and TI nodes.  Six cases (plus six
  -- automatically when re-call with flip).
  --   Oops, there are still also TW and TN nodes...
-------------
  --        WR   WS   TI   TR   TN   TW
  --
  --   WR   Sym  WRWS WRTI WRTR WRTN WRTW
  --
  --   WS   flip Sym  WSTI WSTR WSTN WSTW
  --
  --   TI   flip flip Sym  TITR TITN TITW
  --
  --   TR   flip flip flip Sym  TRTN TRTW
  --
  --   TN   flip flip flip flip Sym  TNTW
  --
  --   TW   flip flip flip flip flip Sym
-----------------
  -- Fifteen cases to consider in all. Can they all be logically reconciled?...
------------------------
  -- WRWS : WR wins (not quite trivial however)
  -- ... if we just let T* be broken for 0.6.0.0, we're done there!
  -- By "broken" meaning, better give an error -- could just erase
  -- the type constraints silently, but what use is that?
  union'asym_3 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WR{} <- pas1, WS{} <- pas2 = {-trace "WRWS" $-} nod1'
#if 0
#if ENFORCE_SAME_ARITY_UNION
   | TR{} <- pas1, WR{} <- pas2 = {-trace "TRWR" $-}
      if csokay then nod1'
      else error $ "unionPat: " ++ if b then "WRTR" else "TRWR" ++ ": encountered arity disparity!"
#else
   | TR{} <- pas1, WR{} <- pas2 = {-trace "TRWR" $-} nod1'
#endif
   | TR{} <- pas1, WS{} <- pas2 = {-trace "TRWS" $-} nod1'
   | TI{} <- pas1, WR{} <- pas2 = {-trace "TIWR" $-} nod2'
   | TI{} <- pas1, WS{} <- pas2 = {-trace "TIWS" $-} nod2'
#if USE_WW_DEEPSEQ
   | TW{} <- pas1, WR{} <- pas2 = {-trace "TWWR" $-} nod1'
   | TW{} <- pas1, WS{} <- pas2 = {-trace "TWWS" $-} nod1'
   | TW{} <- pas1, WW{} <- pas2 = {-trace "TWWW" $-} nod2'
#endif
   | TI{} <- pas1, TR{} <- pas2
      = {-trace "TITR" $-} Node (TR union_as) $ zipWith (union'asym_3 False) cs1 cs2
#endif
   | not b = union'asym_3 True nod2 nod1
   | otherwise = error "unionPats: unexpected failure to (Haskell) pattern-match arguments!"
   where
--  cs1' = map (const emptyPat) cs1
    nod1' = Node pas1' cs1
    pas1' = setPatNodeAttrs pas1 union_as
    union_as = unionAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#if ENFORCE_SAME_ARITY_UNION
    csokay = equilength cs1 cs2
#endif

-------------------------------------------------------------------------------

  -- | Compute the intersection of a list of 'Pattern's.
  --
  -- Where two (or more) homologous 'WR' nodes disagree in arity,
  -- the intersection at that position becomes 'WI'.
  --
  -- /__XXX__ This doesn't yet handle type-constrained 'PatNode's ('TI', 'TR', 'TN' or 'TW'). Other attributes are handled in a haphazard fashion. This is work in progress, but the five un-modified @W*@ node types should be safe./

  intersectPats :: [ Pattern ] -> Pattern
  intersectPats [] = emptyPat
  intersectPats ps = foldr1 (intersect' False) $ force $ map (clearTypeConstraints "intersectPats") ps

  intersect' :: Bool -> Pattern -> Pattern -> Pattern

  -- We hit this coming in every time [modulo compiler/runtime
  -- optimisations], so performance counts.
  intersect' b (Node pas1 cs1) (Node pas2 cs2)
   | WI{} <- pas1               = Node (WI intersect_as) []
   |               WI{} <- pas2 = Node (WI intersect_as) []
   where
    intersect_as = intersectAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)

  -- Symmetric cases:
  intersect' _ nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
#if ENFORCE_SAME_ARITY_UNION
   | WR{} <- pas1, WR{} <- pas2 = {-trace "WRWR" $-}
      if csokay then Node (WR intersect_as) $ zipWith (intersect' False) cs1 cs2
      else error "intersectPat: WRWR: encountered arity disparity!"
#else
   | WR{} <- pas1, WR{} <- pas2 = {-trace "WRWR" $-} Node (WR intersect_as) $ zipWith (intersect' False) cs1 cs2
#endif
   | WS{} <- pas1, WS{} <- pas2 = {-trace "WSWS" $-} Node (WS intersect_as) []
   | WN{} <- pas1, WN{} <- pas2 = {-trace "WNWN" $-} Node (WN intersect_as) []
#if USE_WW_DEEPSEQ
   | WW{} <- pas1, WW{} <- pas2 = {-trace "WWWW" $-} Node (WW intersect_as) []
#endif
   -- XXX T* nodes not handled!
   | otherwise  = intersect'asym_1 False (Node pas1 cs1) (Node pas2 cs2)
   where
    pas1' = setPatNodeAttrs pas1 intersect_as
    pas2' = setPatNodeAttrs pas2 intersect_as
    intersect_as = intersectAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#if ENFORCE_SAME_ARITY_UNION
    csokay = equilength cs1 cs2
#endif

  -- Now the asymmetric cases:
#if USE_WW_DEEPSEQ
  intersect'asym_1 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WW{} <- pas1
      =
--      trace (if b then "**WW" else "WW**") $
        Node pas2' cs2
   | not b      = intersect'asym_1 True nod2 nod1
   | otherwise  = intersect'asym_2 False nod1 nod2
   where
    pas2' = setPatNodeAttrs pas2 intersect_as
    intersect_as = intersectAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#endif

  intersect'asym_2 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WN{} <- pas1
      =
--      trace (if b then "**WN" else "WN**") $
        truncatePat dep (Node (setPatNodeAttrs pas2 intersect_as) cs2)
--      intersect'asym_2_aux dep
   | not b      = intersect'asym_2 True nod2 nod1
   | otherwise  = intersect'asym_3 False nod1 nod2
   where
#if 0
    intersect'asym_2_aux 0 = Node (WI intersect_as) []
    intersect'asym_2_aux 1 = Node (WS intersect_as) []
    intersect'asym_2_aux _ = Node (WN intersect_as) []
#endif
    intersect_as = intersectAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
    dep = depth as1

  intersect'asym_3 b nod1@(Node pas1 cs1) nod2@(Node pas2 cs2)
   | WR{} <- pas1, WS{} <- pas2 = {-trace "WRWS" $-} nod1'
   -- XXX T* nodes not handled!
   | not b = intersect'asym_3 True nod2 nod1
   | otherwise = error "intersectPats: unexpected failure to (Haskell) pattern-match arguments!"
   where
    cs1' = map (const emptyPat) cs1
    nod1' = Node (WR intersect_as) cs1'
    intersect_as = intersectAttrs [as1,as2]
    (as1, as2) = (getPatNodeAttrs pas1, getPatNodeAttrs pas2)
#if ENFORCE_SAME_ARITY_UNION
    csokay = equilength cs1 cs2
#endif

-------------------------------------------------------------------------------

  -- | Return 'True' if the first pattern matches the second (and 'False' otherwise).
  --
  -- Arities must correspond (or the second pattern's node must be wild) for the match to succeed at each __recursive__ 'PatNode' (/i.e./ 'WR' or 'TR').
  -- Matching does not imply spanning; @'flip' 'subPat'@ would work for that.
  --
  -- /__XXX__ This doesn't yet handle type-constrained 'PatNode's ('TI', 'TR', 'TN' or 'TW'), because 'intersectPats' doesn't. Generally speaking, it is difficult to arrive at a good policy for subpattern, union and intersection, when mixed types of nodes with various attribute values are considered!  Other attributes are handled in a haphazard fashion. This is work in progress, but the five un-modified @W*@ node types should be safe./
  --
  -- More formally, we have two \"Subpattern Axioms\":
  --
  -- * __Not More Specifc__ &#8195; A subpattern (of a pattern) is never more specific (/i.e./ less permissive), in terms of what values it will match.
  --
  -- * __Not More Forcing__ &#8195; A subpattern never has greater forcing potential.
  --
  -- And a /proper/ subpattern will always be strictly more permissive or
  -- less forcing (or both).
  subPat :: Pattern -> Pattern -> Bool
  subPat p pp = p == intersectPats [p, pp]  -- probably faster on avg.
--subPat p pp = pp == unionPats [p, pp]

-------------------------------------------------------------------------------

  -- I guess I don't really clear the typeConstraints list,
  -- only toggle doConstrainType to False.
  clearTypeConstraints :: String -> Pattern -> Pattern
  clearTypeConstraints s (Node pas cs)
   = Node pas' $ map (clearTypeConstraints s) cs
   where
    as = getPatNodeAttrs pas
    as' = if doConstrainType as
--        then error "unionPats: type constraints unsupported!"
--        then trace "unionPats: warning: type constraints unsupported (cleared!)" as
          then unsafePerformIO $ do
                 firstwarnpassed <- readIORef firstWarningPassed
                 modifyIORef' firstWarningPassed (const True)
                 if firstwarnpassed
                   then return as
                   else trace (s ++ ": warning: type constraints unsupported (cleared!)")
                        $! return as
          else as
    as'' = as' { doConstrainType = False }
    pas' = setPatNodeAttrs pas as''

-------------------------------------------------------------------------------

  -- XXX Note: Concerning the doSpark field, there is a deeper issue
  -- lurking here. Parallelisation and strictification are orthogonal
  -- concerns, so trying to treat them uniformly can be awkward...

  unionAttrs :: [PatNodeAttrs] -> PatNodeAttrs
  unionAttrs [] = error "unionAttrs: called with empty list.\nPlease determine which library has called this function erroneously,\nand file a bug report or contact the maintainer of that project."
  unionAttrs as = foldl1 unionAttrs' as
  unionAttrs' :: PatNodeAttrs -> PatNodeAttrs -> PatNodeAttrs
  unionAttrs'
    (PatNodeAttrs
      uniq1
      dep1
      dotys1
      tys1
      dly1 dlyus1
#if USE_PAR_PATNODE
      spk1
#endif
#if USE_PSEQ_PATNODE
      pseq1 pseqperm1
#endif
#if USE_TRACE_PATNODE
      tr1
#endif
#if USE_PING_PATNODE
      ping1 pingtid1
#endif
#if USE_DIE_PATNODE
      die1
#endif
#if USE_TIMING_PATNODE
      timing1 ts1 parent_ts1 delta_ts1
#endif
    )
    (PatNodeAttrs
      uniq2
      dep2
      dotys2
      tys2
      dly2 dlyus2
#if USE_PAR_PATNODE
      spk2
#endif
#if USE_PSEQ_PATNODE
      pseq2 pseqperm2
#endif
#if USE_TRACE_PATNODE
      tr2
#endif
#if USE_PING_PATNODE
      ping2 pingtid2
#endif
#if USE_DIE_PATNODE
      die2
#endif
#if USE_TIMING_PATNODE
      timing2 ts2 parent_ts2 delta_ts2
#endif
    )
     = PatNodeAttrs
         (combineUniqueIDs uniq1 uniq2)
         (max dep1 dep2)
         (dotys1||dotys2)  -- SAME as intersection [?]
         (unionTys tys1 tys2)
         (dly1||dly2)  -- SAME as intersection [?]
         (combineDelays True dly1 dly2 dlyus1 dlyus2)  -- DIFFERENT for union and intersection [?]
#if USE_PAR_PATNODE
         (spk1||spk2)  -- seems the best choice for parallelism union semantics
#endif
#if USE_PSEQ_PATNODE
         (pseq1&&pseq2)  -- DIFFERENT for union and intersection [??]
         (combinePseqPerms pseqperm1 pseqperm2)  -- XXX SAME as intersection...
#endif
#if USE_TRACE_PATNODE
         (combineTraces tr1 tr2)  -- same for union and intersection
#endif
#if USE_PING_PATNODE
         (combinePings ping1 ping2)  -- same for union and intersection
         (combinePingParentTIDs pingtid1 pingtid2)  -- same for union and intersection
#endif
#if USE_DIE_PATNODE
         (die1||die2)  -- same for union and intersection
#endif
#if USE_TIMING_PATNODE
         -- same for union and intersection [?]
         -- XXX This is bogus, so it would be nice if it would issue a warning.
         -- We'd want to issue the warning only once per run, I think...
         (False) (0) (0) (0)
#endif

  intersectAttrs :: [PatNodeAttrs] -> PatNodeAttrs
  intersectAttrs [] = error "intersectAttrs: called with empty list.\nPlease determine which library has called this function erroneously,\nand file a bug report or contact the maintainer of that project."
  intersectAttrs as = foldr1 intersectAttrs' as
  intersectAttrs' :: PatNodeAttrs -> PatNodeAttrs -> PatNodeAttrs
  intersectAttrs'
    (PatNodeAttrs
      uniq1
      dep1
      dotys1
      tys1
      dly1 dlyus1
#if USE_PAR_PATNODE
      spk1
#endif
#if USE_PSEQ_PATNODE
      pseq1 pseqperm1
#endif
#if USE_TRACE_PATNODE
      tr1
#endif
#if USE_PING_PATNODE
      ping1 pingtid1
#endif
#if USE_DIE_PATNODE
      die1
#endif
#if USE_TIMING_PATNODE
      timing1 ts1 parent_ts1 delta_ts1
#endif
    )
    (PatNodeAttrs
      uniq2
      dep2
      dotys2
      tys2
      dly2 dlyus2
#if USE_PAR_PATNODE
      spk2
#endif
#if USE_PSEQ_PATNODE
      pseq2 pseqperm2
#endif
#if USE_TRACE_PATNODE
      tr2
#endif
#if USE_PING_PATNODE
      ping2 pingtid2
#endif
#if USE_DIE_PATNODE
      die2
#endif
#if USE_TIMING_PATNODE
      timing2 ts2 parent_ts2 delta_ts2
#endif
    )
     = PatNodeAttrs
         (combineUniqueIDs uniq1 uniq2)
         (min dep1 dep2)
         (dotys1||dotys2)  -- SAME as union [?]
         (intersectTys tys1 tys2)
         (dly1||dly2)  -- SAME as union [?]
         (combineDelays False dly1 dly2 dlyus1 dlyus2)  -- DIFFERENT for union and intersection [?]
#if USE_PAR_PATNODE
         (spk1||spk2)  -- seems the better choice
--       (spk1&&spk2)  -- dubious parallelism intersection semantics
#endif
#if USE_PSEQ_PATNODE
         (pseq1||pseq2)  -- DIFFERENT for union and intersection [??]
         (combinePseqPerms pseqperm1 pseqperm2)  -- XXX SAME as union...
#endif
#if USE_TRACE_PATNODE
         (combineTraces tr1 tr2)  -- same for union and intersection
#endif
#if USE_PING_PATNODE
         (combinePings ping1 ping2)  -- same for union and intersection
         (combinePingParentTIDs pingtid1 pingtid2)  -- same for union and intersection
#endif
#if USE_DIE_PATNODE
         (die1||die2)  -- same for union and intersection
#endif
#if USE_TIMING_PATNODE
         -- same for union and intersection [?]
         -- XXX This is bogus, so it would be nice if it would issue a warning.
         -- We'd want to issue the warning only once per run, I think...
         (False) (0) (0) (0)
#endif

-------------------------------------------------------------------------------

  -- The idea here is that, PatNodeAttrs only carries a Bool for each
  -- of combineTraces and combinePings, and the actual content of the
  -- log string or Exception message is a matter to be determined in
  -- the context that the logging/exception is triggered (demanded).

#if USE_TRACE_PATNODE
  combineTraces :: Bool -> Bool -> Bool
  combineTraces = (||)  -- for now, at least
#endif

#if USE_PSEQ_PATNODE
  combinePseqPerms :: Maybe [Int] -> Maybe [Int] -> Maybe [Int]
  combinePseqPerms mpsp1 mpsp2
   | isNothing mpsp1  = mpsp2
   | isNothing mpsp2  = mpsp1
   | mpsp1 == mpsp2   = mpsp1
   | otherwise        = trace "WARNING: combinePseqPerms: Incompatible permutations discarded (using id)." Nothing
#endif

#if USE_PING_PATNODE
  combinePingParentTIDs :: Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId
  combinePingParentTIDs mt1 mt2
   = if isNothing mt1 && isNothing mt2
     then Nothing
     else if isJust mt1 && isJust mt2 && t1 == t2
          then Just t1
#if 1
          else
#if 1
               trace
                 (    "combinePings: TID's differ: "
                   ++ show mt1 ++ " " ++ show mt2
                   ++ "\nCombining by choosing " ++ show mt1 ++ "!" )
#endif
                 mt1
#else
          -- XXX This shouldn't be "error"!
          else error $    "combinePings: TID's differ: "
                       ++ show mt1 ++ " " ++ show mt2
#endif
--        else Nothing
   where (t1,t2) = (fromJust mt1,fromJust mt2)

  combinePings :: Bool -> Bool -> Bool
  combinePings = (||)  -- for now, at least
#endif

  combineUniqueIDs :: Int -> Int -> Int
  combineUniqueIDs u1 u2
   | u1 == u2   = u1
   | otherwise  =
#if 0
-- Seeing as this will happen 99% of the time, it's a bit much
-- to issue a warning to console!
                  trace
                    ( "combineUniqueIDs: uniqueID's differ: "
                       ++ show u1 ++ " " ++ show u2 )
#endif
                    0

  -- Rather dubious.
  combineDelays :: Bool -> Bool -> Bool -> Int -> Int -> Int
  combineDelays from_union dly1 dly2 dlyus1 dlyus2
   | not dly1 && not dly2  = 0
   | not dly1              = dlyus2
   | not dly2              = dlyus1
   | from_union            = min dlyus1 dlyus2
   | otherwise             = max dlyus1 dlyus2

-------------------------------------------------------------------------------

  -- Probably overkill for typical lengths.
  -- Would pay to special case for some short lists.
  -- Optimisations come later.

  unionTys :: [String] -> [String] -> [String]
  unionTys ss1 ss2 = nubsort $ ss1 ++ ss2

  nubsort :: Ord a => [a] -> [a]
  nubsort = map head . group . sort

  intersectTys :: [String] -> [String] -> [String]
  intersectTys ss1 ss2 = intersect (nubsort ss1) (nubsort ss2)

-------------------------------------------------------------------------------

#if ENFORCE_SAME_ARITY_UNION
  -- Try to determine whether then lengths of the lists are
  -- the same or not without being too inefficient...
  {-# INLINE equilength #-}
  equilength :: [a] -> [b] -> Bool
#if 0
-- XXX But ... I feel like best practise is to use "length cs1 == length cs2",
-- since who knows, the compiler could optimise it into some sort of quasi-
-- list internal structure with O(1) length function.  That is not too
-- far fetched at all.  (Complexities as reported in the standard libraries
-- need not be strictly observed -- actual evaluations may exhibit lower
-- complexity without violating any specification...)  And if this happens,
-- then we've blocked the optimisation, so we get a 0.5*O(n) average case
-- algorithm instead of an O(n), but missed out on O(1)...
--   The only really safe way is to implement using your choice of
-- container with O(1) length.  Or of course, you could attempt to
-- maintain the length as associated data (extra field)...
  equilength (x:xs) (y:ys) = equilength xs ys
  equilength [] [] = True
  equilength _ _ = False
#else
  equilength cs1 cs2 = length cs1 == length cs2  -- ouch(?)
#endif
#endif

  zipWith_ :: String -> (a -> b -> c) -> [a] -> [b] -> (Bool,[c])
  zipWith_ caller f xs ys
   | b          = (b,zs)
   | otherwise  = trace (caller ++ ": node arity disparity!") (b,zs)
   where
    (b,zs') = zipWith_' f xs ys []
    zs = reverse zs'
  zipWith_' :: (a -> b -> c) -> [a] -> [b] -> [c] -> (Bool,[c])
  zipWith_' f (x:xs) (y:ys) acc = zipWith_' f xs ys (f x y : acc)
  zipWith_' _ [] [] acc = (True,acc)
  zipWith_' _ _ _ acc = (False,acc)

-------------------------------------------------------------------------------

  -- | Given an integer depth and a pattern, truncate the pattern to
  -- extend to at most this requested depth.
  --
  -- Nodes in the truncated pattern which were 'WR' and are now leaves,
  -- are changed to 'WI'.
  --
  -- /__XXX__ Note that @*N@ and @*W@ nodes are retained, so if you are using those then \"extend to at most this depth\" does not mean the forcing potential of the pattern is at most that depth...  It would be quite possible to improve this, so @*N@ (and @*W@ nodes, obviously) are \"weakened\" (depth is reduced) so that they end at the truncation depth, regardless of how far up they reside.  In particular, any @*N@ or @*W@ node at truncation depth could be replaced by @WS@.  This works well as all these node types are arity-agnostic./
  truncatePat :: Int -> Pattern -> Pattern
  truncatePat n node@(Node pas chs)
   | n <= 0     = if isWI pas then Node (WI as) []
                  else Node (WR as) $ map (const emptyPat) chs
--- | n <= 0     = Node (WS as) []
   | otherwise  = Node pas $ map (truncatePat (-1+n)) chs
   where as = getPatNodeAttrs pas

-------------------------------------------------------------------------------

  -- | There is no @Nil@ in the @Pattern@ type, but a single 'WI' node as
  -- empty pattern is a dependable way to ensure that the empty pattern
  -- never forces anything. This sets 'PatNodeAttrs' to 'emptyPatNodeAttrs'.
  emptyPat :: Pattern
  emptyPat = Node (WI emptyPatNodeAttrs) []  -- this should do it!

-------------------------------------------------------------------------------

#if ! HASKELL98_FRAGMENT
  -- | Obtain a lazy, potentially infinite pattern, matching the shape
  -- of an arbitrary term (value expression).
  --
  -- There is only one kind of 'PatNode' employed here, 'WR'.
  --
  -- The 'Pattern' is extended indefinitely on demand.  In case the
  -- term has leaves, these will be 'WR' nodes with empty child lists
  -- in the corresponding pattern.
  --
  -- __Caveat__: Note that @mkPat@ gives counter-intuitive results when used
  -- on rose trees, in particular on 'Pattern' itself.
  -- For example, a rose tree with a single node will have a 3-node /\\ shape.)
  -- Formally, 'mkPat' is not idempotent on 'Pattern's, but rather grows
  -- without bound when iterated. This shouldn't be an issue in practise.
  mkPat :: forall d. Data d => d -> Pattern
  mkPat = f . shapeOf
   where
    f (Node _ cs) = Node pn $ map f cs
     where
      pn = WR emptyPatNodeAttrs
---   pn = let as = emptyPatNodeAttrs in if null cs then WS as else WR as

  -- | Obtain a lazy, finite pattern, matching the shape of
  -- an arbitrary term, but only down to at most depth @n@.
  --
  -- Satisfies @'forcep' . 'mkPatN' n = 'forcen' n@. /(Later: I kinda doubt that is true in full generality?... Although it does convey the idea.)/
  --
  -- Unlike 'mkPat', three pattern node contexts arise here:
  --
  -- * those corresponding to actual leaf (nullary) nodes of the term
  --
  --     * these are &#8195; @Node WR []@
  --
  -- * interior nodes of the pattern corresponding to interior nodes of the term
  --
  --     * these are &#8195; @Node WR chs@ &#8195; where @chs@ are the child subpatterns of this interior pattern node
  --
  -- * leaf nodes of the pattern corresponding to interior nodes of the term, that is, non-leaf nodes of the term which are at a depth @n@ of nested constructor applications.
  --
  --     * these are &#8195; @Node WR chs'@ &#8195; where &#8195; @chs' = map (const $ Node WI []) chs = map (const emptyPat) chs@
  --     * this essentially says we're allowed to know the arity of the node, but aside from this cardinal number we know nothing whatsoever concerning the child subpatterns and are not even permitted to evaluate their heads
  --
  -- All interior nodes are 'WR', and all leaf nodes are 'WI'; 'WS' never arise.
  --
  -- See caveat in the 'mkPat' documentation.
  mkPatN :: forall d. Data d => Int -> d -> Pattern
  mkPatN n = f n . shapeOf
   where
    f n (Node _ cs)
     | 0 == n     = if null cs
                    then Node pn []
                    else Node pn $ map (const emptyPat) cs
     | otherwise  = Node pn $ map (f (-1+n)) cs
     where
      pn = WR emptyPatNodeAttrs
#endif

-------------------------------------------------------------------------------

  -- | Elide all leaves which have no non-leaf sibling.
  -- We want the pattern to still match the same value, only less of it.
  -- Merely eliding all leaves would, in most cases, cause match failure,
  -- so we have to be a bit more subtle.  There are some arbitrary
  -- decisions about the relaxation route through the lattice.
  -- (Refer to the source for details.)
  --
  -- More formally, we have some \"Shrinkage Axioms\".  The first
  -- two are really just the \"Subpattern Axioms\" again, that is,
  -- shrinkage is always to a subpattern in our sense of the
  -- word (see also 'subPat'):
  --
  -- * __Not More Specifc__ &#8195; Shrinkage is never towards a more specific (/i.e./ less permissive) pattern.
  --
  -- * __Not More Forcing__ &#8195; Shrinkage is never towards a pattern with greater forcing potential.
  --
  -- And additionally, for finite patterns only:
  --
  -- * __Non-Constancy__ &#8195; A finite pattern is constant under shrinkage /iff/ the pattern is trivial (emptyPat, \".\", Node WI []). However, infinite patterns have other limits.  For instance, the infinite pattern @'concat' $ 'repeat' "(."@ (yes you can do that!) is already stationary under shrinkage.
  --
  -- * __Convergence__ &#8195; On iteration, shrinkage of finite patterns reaches the trivial pattern in a number of steps proportional to the size of the initial pattern argument.  (Actually, @*N@ and @*W@ nodes can make this larger.)  However, in the case of infinite patterns, all bets are off: Simple examples exist which converge immediately, or which continue shrinking indefinitely.)
{--}
  -- XXX This has a bug -- it introduces WR []'s when the original
  -- node arity was not zero! The progression should be (in the
  -- newest syntax) something like:
  --   (.(!.))
  --   (.(..))
  --   (.!)
  --   (..)
  --   !
  --   .
  shrinkPat :: Pattern -> Pattern
  shrinkPat (Node pas cs)
#if USE_PAR_PATNODE
   -- take de-parallelisation as shrinkage
   -- XXX So should we take pseq-isation as further shrinkage?
   | doSpark as  = let as' = as { doSpark = False } in
                   case pas of
                    WI{} -> Node (WI as') []
                    WS{} -> Node (WI as') []
                    WR{} -> Node (WR as') cs
                    WN{} -> Node (WN as') []
#if USE_WW_DEEPSEQ
                    WW{} -> Node (WW as') []
#endif
#endif
   | WI{} <- pas  = Node (WI as) []  -- can't shrink (evt'ly elided from parent)
   | WS{} <- pas  = Node (WI as) []  -- may as well
   | WN{} <- pas
      = let n = depth as
            n' = -1+n
        in if n' <= 0 then Node (WI as) []
           else if n' == 1 then Node (WS as) []
           else let as' = as { depth = n' } in Node (WN as') []
#if USE_WW_DEEPSEQ
   | WW{} <- pas  = Node (WN as { depth = 5 }) []  -- XXX arbitrary hardcode
#endif
   -- take un-type-constrained as shrinkage (pattern becomes less specific)
   -- XXX Later: This is wrong. It is less specific, but it HAS MORE FORCING
   -- POTENTIAL (except in TI case).
   | TI{} <- pas  = Node (WI as) []
   | TR{} <- pas  = Node (WR as) cs
   | TN{} <- pas  = Node (WN as) []
#if USE_WW_DEEPSEQ
   | TW{} <- pas  = Node (WW as) []
#endif
#if SHRINK_TO_EMPTY_WR
   | WR{} <- pas , null cs  = Node (WI as) []
#endif
   -- If this node has any grandchildren, recurse on the children.
   | not $ null $ filter (\ (Node q gcs) -> not $ null gcs) cs
      = Node pas $ map shrinkPat cs
   -- At this point we know this node has no grandchildren.
   -- Check if all children are insulator nodes.
   | null $ filter (\ (Node pas _) -> case pas of { WI{} -> False ; _ -> True }) cs
      = case pas of
         -- Must go to WI, since .{##} -> . is /not/ a lazification.
         -- Later: Yes it is, isn't it?? Dot is a less constrained pattern...
#if SHRINK_TO_EMPTY_WR
         WR{} -> Node (WR as) []
#else
         WR{} -> Node (WI as) []
#endif
         -- We cannot make this simple TR -> WR (drop type constraints),
         -- since, while less specific, it forces more, violating the
         -- "second axiom of shrinkage".
         TR{} -> let as' = as { typeConstraints = [] } in Node (WI as') []  -- better?
--       TR{} -> Node (WI as) []  -- sic
         _ -> error "shrinkPat: unexpected!"
   | otherwise
      = Node pas $ map shrinkPat cs  -- still contains shrinkable children
   where
    as = getPatNodeAttrs pas

-------------------------------------------------------------------------------

#if ! HASKELL98_FRAGMENT
  -- | Grow all leaves by one level within the shape of the provided value.
  -- This is intended to be used with \"plain\" patterns, /i.e./ those
  -- containing only 'WR' and 'WI' nodes. (There is no code enforcing this.)
  -- A new growth node always replaces a 'WI' (leaf) node with a 'WR' node
  -- bearing the suitable number of 'WI' children to encode arity (see
  -- 'mkPat' for general commentary about this).
  growPat :: forall d. Data d => Pattern -> d -> Pattern
  growPat pat x = growPat' pat $ shapeOf x
  growPat' :: Pattern -> Shape -> Pattern
  growPat' (Node pas cs) (Node q ds)
#if 0
#elif 1
   | null cs    = let subleaf = Node (WI $ getPatNodeAttrs pas) [] in
                       let pas' = WR $ getPatNodeAttrs pas in
                            Node pas' $ map (const subleaf) ds
#elif 0
   | null cs    = let subleaf = Node (WI $ getPatNodeAttrs pas) []
                      pas' = WR $ getPatNodeAttrs pas in
                        Node pas' $ map (const subleaf) ds
#elif 0
   -- Mysteriously stopped working; parse error on = in second def.
   -- How many bizillion times have I used this layout without a problem?
   -- And many times compiled in the last week even. Now I'm worried
   -- about a hundred unknown sites in other code that use this.
   -- This has been one of the worst days of my life in some ways.
   | null cs    = let subleaf = Node (WI $ getPatNodeAttrs pas) []
                      pas' = WR $ getPatNodeAttrs pas
                  in Node pas' $ map (const subleaf) ds
#endif
   | otherwise  = Node pas $ zipWith growPat' cs ds
#endif

-------------------------------------------------------------------------------

  -- | This creates a new 'WR' node, the common root, with 'PatNodeAttrs' set
  -- to 'emptyPatNodeAttrs'.  The argument patterns become the children of
  -- the root (order is preserved).
  liftPats :: [ Pattern ] -> Pattern
  liftPats ps = Node (WR emptyPatNodeAttrs) ps

-------------------------------------------------------------------------------

  -- | Add children to a node (interior or leaf) of the target.
  -- The first argument is target pattern, the second is a path (0-based
  -- indexing) from the root of the target to any choice node,
  -- and the third is a list of subpatterns for insertion, along with the
  -- indices of the insertion slots.
  -- Indices range through @-1,0..n@, where @n@ is the number of
  -- existing children, and @-1@ is short for @n@ (so you don't need
  -- to count off the children to append!).
  -- Indices are always relative to the original target as it was received.
  splicePats :: Pattern -> [Int] -> [ (Int, Pattern) ] -> Pattern
  splicePats target path isibs'
--- | isibs /= isibs'    = error "splicePats: siblings to be inserted must be indexed in increasing order"
--- | not uniqueIdxs     = error "splicePats: siblings to be inserted must be uniquely indexed"
--- | not $ isPath path  = error "splicePats: path malformed"
   | otherwise          = splice' target path isibs
   where
--  uniqueIdxs = length isibs == ( length $ nub $ map fst isibs )
    isibs = sortBy comp isibs'  -- questionable solution
     where
      comp (x1,_) (x2,_) = compare x1 x2
    -- Now, what's the clever way to do this? it's ugly manual
    -- recursion if don't think of something nicer. (This is
    -- the ugly manual recursion!)
    splice' :: Pattern -> [Int] -> [(Int,Pattern)] -> Pattern
    splice' (Node pas cs) [] isibs  -- end of path chain
     | maximum (map fst isibs) > ncs  = error "splicePats: insertion indices (0-based) must be\nno greater than the number of children"
     | or $ map ( (<(-1)) . fst ) isibs  = error "splicePats: insertion indices must be non-negative, or -1"
---  | or $ map ( (<0) . fst ) isibs  = error "splicePats: insertion indices must be non-negative"
     | not ( isWR pas || isTR pas )  = error $ "splicePats: path ends in non-recursive node type " ++ show pas
     | otherwise  = {-trace "**1**" $-} Node pas $ f 0 cs isibs_
     where
      ncs = length cs
      -- XXX I'm trying to remember why had isibs of -1 as special.
      -- (They're now filtered upstream of here in any case.)
      isibs_ = let lst = takeWhile ((== -1) . fst) isibs in
               drop (length lst) isibs ++ map (\ (x,y) -> (ncs,y)) lst
      f n cs [] = cs
      f n [] isibs_remaining = map snd isibs_remaining
--    f n [] isibs_remaining = error $ "splicePats: (2) path escapes target: " ++ show isibs_remaining  -- shouldn't happen
      f n lst1@(c:cs) lst2@((i,s):iss)
---    | trace ("**3**"++show lst1++" "++show lst2) False  = undefined
       | ii == n    = map snd ss ++ (c : f (1+n) cs ss')
       | otherwise  =                c : f (1+n) cs lst2
       where (ss,ss') = span (\ (i,s) -> i == n) lst2
             ii = i
--           ii = if i < 0 then ncs-(-i) else i
    splice' (Node pas cs) (i:is) isibs
---  | trace ("**4** "++show i++" / "++show cs++" / "++show pathcs) False  = undefined
     | null cs  = error "splicePats: path escapes target (depth)"
     | length cs < 1+i  = error "splicePats: path escapes target (breadth)"
     | null ccsR  = error "splicePats: (2) path escapes target (depth)"
-- Shouldn't be possible?
---  | not ( isWR pas || isTR pas )  = error $ "splicePats: path contains non-recursive node type " ++ show pas
     | otherwise  = {-trace "**2**" $-} Node pas (csL ++ [splice' c is isibs] ++ csR)
     where
      (c:csR) = ccsR
      (csL,ccsR) = splitAt i cs

-------------------------------------------------------------------------------

  -- | Elide children of a node (interior or leaf) of the target.
  -- The first argument is target pattern, the second is a path (0-based
  -- indexing) from the root of the target to any choice node,
  -- and the third is a list of child indices for elision.
  -- Indices range through @-1,0..n-1@, where @n@ is the number of
  -- existing children, and @-1@ is short for @n-1@ (so you don't need
  -- to count off the children to elide the rightmost).
  -- Indices are always relative to the original target as it was received.
  elidePats :: Pattern -> [Int] -> [Int] -> Pattern
  elidePats target path isibs' = elide' target path isibs
   where
    isibs = sortBy comp isibs'  -- questionable solution
     where comp x1 x2 = compare x1 x2
    -- (See comment in splicePats.)
    elide' :: Pattern -> [Int] -> [Int] -> Pattern
    elide' (Node pas cs) [] isibs  -- end of path chain
     | maximum isibs >= ncs  = error "elidePats: elision indices (0-based) must be\nless than the number of children"
     | or $ map (<(-1)) isibs  = error "elidePats: elision indices must be non-negative or -1"
---  | or $ map (<0) isibs  = error "elidePats: elision indices must be non-negative"
     | otherwise  = {-trace "**1**" $-} Node pas $ f 0 cs isibs_
     where
      ncs = length cs
      -- XXX I'm trying to remember why had isibs of -1 as special.
      -- (They're now filtered upstream of here in any case.)
      isibs_ = let lst = takeWhile (== -1) isibs in
               drop (length lst) isibs ++ map (\ x -> (-1+ncs)) lst
      f n cs [] = cs
      f n [] isibs_remaining = error $ "elidePats: (3) path escapes target: " ++ show isibs_remaining  -- shouldn't happen
      f n lst1@(c:cs) lst2@(i:iss)
---    | trace ("**3**"++show lst1++" "++show lst2) False  = undefined
       | ii == n    =     f (1+n) cs ss'
       | otherwise  = c : f (1+n) cs lst2
       where (ss,ss') = span (\ i -> i == n) lst2
             ii = i
--           ii = if i < 0 then ncs-(-i) else i
    elide' (Node pas cs) (i:is) isibs
---  | trace ("**4** "++show i++" / "++show cs++" / "++show pathcs) False  = undefined
     | null cs  = error "elidePats: path escapes target (depth)"
     | length cs < 1+i  = error "elidePats: path escapes target (breadth)"
     | null ccsR  = error "elidePats: (2) path escapes target (depth)"
     | otherwise  = {-trace "**2**" $-} Node pas (csL ++ [elide' c is isibs] ++ csR)
     where
      (c:csR) = ccsR
      (csL,ccsR) = splitAt i cs

-------------------------------------------------------------------------------

  -- | Select a leaf at random, and elide it.
  -- In order to achieve fairness, the node probabilities are
  -- weighted by nodes in branch.
  -- The path arg can "focus" the stochastic erosion to only
  -- consider leaves beneath a given node.
{--}
-- XXX Later: I don't understand. When would you want to
-- allow the number of children at a node to change (except
-- possibly to zero)?... Question applies also to splicePats
-- and to elidePats.
-- XXX It would be better if the weighting could be done once,
-- then maintained, but will have to see how it performs...
-- XXX Hey! This doesn't even need to call elidePats.
-- XXX Later: It would be nice if this could call shrinkPat
-- (or equivalent) on the leaves finally selected, so they
-- decrease forcing at finer granularity, eg. *3 -> *2 (not *3 -> #
-- or rather outright elided ... this is a different process though;
-- need to decide whether you want to let that arity change or not...)
  erodePat :: StdGen -> [Int] -> Pattern -> (Pattern, StdGen)
  -- Just descend the path, reconstructing recursively (usual thing),
  -- and when get to the node addressed by path, then choose (fair) your
  -- leaf under that.
  erodePat g (h:t) (Node pas chs)
   = ( Node pas $ left ++ [ ch'' ] ++ right , g' )
   where
    ch'' = ch'
--  ch'' = (\ (Node (r,pas) chs) -> Node r chs) ch'
    (ch',g') = erodePat g t lucky
    (left,lucky:right) = splitAt h chs
  erodePat g [] pat = (pat', g')
--erodePat g [] pat = trace (showRose wpat ++ "\n" ++ showRose (weightedRose pat)) $ (pat', g')
   where
    pat' = fst $ unzipRose wpat'
    !_ = probDensRose pat
--  !_ = force $ probDensRose pat
    (wpat', g') = f g wpat
--  !wpat@(Node pas chs) = probDensRose pat
    wpat@(Node pas chs) = probDensRose pat
    f :: StdGen -> Rose (PatNode,Double) -> (Rose (PatNode,Double), StdGen)
    f g (Node pas chs)
     | isNothing mh  = ( Node pas chs, g )  -- ??
     | null chs   = ( Node pas [], g )
---  | null chs   = ( Node pas [], g'' )
     | null gchs  = ( Node pas $ left ++ [ Node (WI emptyPatNodeAttrs,1.0) [] ] ++ right , g'' )
---  | null gchs  = ( Node pas $ left ++ right , g'' )
     | otherwise  = ( Node pas $ left ++ [ ch' ] ++ right , g'' )
     where
-- XXX I see; I have a logic error.
-- Cyclical definition.
-- null gchs
-- but gchs depends on lucky
-- and lucky depends on ... [?]
      (Node _ gchs) = lucky
      (ch',g'') = f g' lucky
      chprobs = map (\ (Node (_,pas) _) -> pas) chs
      mh = lucky_child 0 0.0 chprobs
#if 1
      h = fromJust mh
#else
      h | isNothing mh  = error "UNEXPECTED!"  -- definitely get here
        | otherwise     = fromJust mh
#endif
      (left,lucky:right) = splitAt h chs
--    !_ = trace ("r=" ++ show r) $ ()
--    (r,g') = trace "HERE!" $ randomR (0,1) g
      (r,g') = randomR (0,1) g
      lucky_child :: Int -> Double -> [Double] -> Maybe Int
      lucky_child idx acc [] = Nothing
      lucky_child idx acc (cp:cps)
---    | trace (" >>> " ++ show acc ++ "  " ++ show acc') $ False = undefined
       | acc' >= r   = Just idx
       | otherwise  = lucky_child (1+idx) acc' cps
       where
        acc' = acc + cp

-------------------------------------------------------------------------------

  -- See the sai-shape-syb package for an API full of this sort of thing.

#if ! HASKELL98_FRAGMENT
  type Shape = Rose ()
  shapeOf :: forall d. Data d => d -> Shape
  shapeOf = ghom $ const ()
  ghom :: forall r d. Data d => GenericQ r -> d -> Rose r
  ghom f x = foldl k b (gmapQ (ghom f) x)
   where
     b = Node (f x) []
     k (Node r chs) nod = Node r (chs++[nod])
#endif

  probDensRose :: Rose r -> Rose (r, Double)
  probDensRose = probDensRose' 1.0 . weightedRose
  probDensRose' :: Double -> Rose (r, Int) -> Rose (r, Double)
--probDensRose' p (Node (r,w) []) = Node (r,p) []  -- (helps avoid div-by-zero)
  probDensRose' p (Node (r,w) chs)
   = Node (r,p) $ zipWith probDensRose' chprobs chs
   where
    chwts   = map (\ (Node (_,w) _) -> w) chs
    chwtsum = foldl' (+) 0 chwts
    normfac = 1 / fromIntegral chwtsum
--- !_ = trace (" *** " ++ show chprobs)
    chprobs = map (\ (Node (_,w) _) -> normfac * (fromIntegral w)) chs

  weightedRose :: Rose r -> Rose (r, Int)
  weightedRose (Node r chs) = foldl k' b (map weightedRose chs)
   where
     k = (\ (r,w) (r',w') -> (r,w+w'))
     b = Node (r,1) []
     k' (Node rw chs) nod@(Node rw' _) = Node (rw `k` rw') (chs++[nod])

  unzipRose :: Rose (r, s) -> (Rose r, Rose s)
  unzipRose (Node (x,y) ns) = (Node x xns, Node y yns)
   where
    (xns,yns) = unzip $ map unzipRose ns

  showRose :: Show r => Rose r -> String
  showRose = show' 0
   where show' n (Node r chs)
           = indent n ++ show r ++ "\n" ++ concatMap (show' (1+n)) chs
              where indent n = concat $ replicate n "| "

-------------------------------------------------------------------------------

