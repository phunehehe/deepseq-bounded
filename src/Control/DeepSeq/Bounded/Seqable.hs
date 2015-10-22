
-------------------------------------------------------------------------------

-- XXX Although there's nothing wrong with this, nevertheless
-- GSeqable works so well, and is probably more performant,
-- so going to just alias that here.

-------------------------------------------------------------------------------

-- XXX This no longer uses classes, so make sure that's
-- reflected in all comments and documentation (here, and
-- in other places!)...

-------------------------------------------------------------------------------

-- XXX This is finally working -- once I moved to DEPTH_TWO branches,
-- ALL the tests passed, first go! (So nice to see!)
--
-- BUT (XXX), it would be really great if we can be CERTAIN that
-- all "rnfn 2" calls are INLINED (RECURSIVELY, as it's not deep,
-- in case there was any recursion).
--
-- Unfortunately, GHC can't inline recursive functions [?],
-- but something should be done, and I hope it's not going
-- back to class/instances here in Seqable...

-------------------------------------------------------------------------------

-- XXX Comments were preliminary, and are a bit rotten...

-- The plan for this is an optimised, specialised version of NFDataN.
-- It will handle only two possible depths (so it takes one Bit of
-- information for it's depth argument, only).
--
-- I'm not yet certain this will be generally useful, but it
-- is closer to the model of what I would like to see offered
-- by the Haskell RTS itself...
--
-- Semantically:
--
--   seq_ :: Bit -> a -> b -> b
--   seq_ 0 x y = deepseqn 1 x y
--   seq_ 1 x y = deepseqn 2 x y
--   seq_ n _ _ = error.
--
-- The difference is that seq_ has been specialised and optimised
-- for the fact that it's only defined for two, shallow depths.
-- Just enough to prime recursion.
--
-- This is "only useful" when multiple seq_'s are working
-- in tandem within an extended expression/value.
--
-- This can be controlled dynamically (see <seqaid> project);
-- and ideally it would be part of the RTS...
--
-- Another bonus is, all of Seqable.hs and GSeqable.hs
-- are in the HASKELL98_FRAGMENT. [Later: I don't understand
-- what I'm getting at. Seqable.hs is H98, but only with
-- JUST_ALIAS_GSEQABLE set to False (which is not the default
-- at present)...

-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}  -- specified in .cabal default-extensions

#define DEPTH_TWO 1

#define USE_NFDATA_SUPERCLASS 0

-------------------------------------------------------------------------------

  -- Later: I'm not so sure about this, actually; is the arithmetic
  -- on n actually not piling up thunks, without the bang-patterns?
  --
  -- It would be easy to get rid of the bang-patterns.
  -- The Complex instance is done as an example.
  -- If you do go with the case's, probably want -fno-warn-name-shadowing.
  {-# LANGUAGE BangPatterns #-}
  {-# OPTIONS_GHC -fno-warn-name-shadowing #-}

  {-# LANGUAGE Rank2Types #-}

  {-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Seqable
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (uses SOP)
--
-- This module provides generic functions 'rnf_', 'force_' and 'seq_',
-- for efficient switching between @n=0@ and @n=2@ in the corresponding
-- 'rnfn n', 'forcen n' and 'deepseqn n' (of 'NFDataN').  This is useful for
-- connecting units of forcing (propagating demand).  It was motivated
-- for use with auto-instrumentation, where 'seq_' can be injected
-- at every node of the AST (refer to the <seqaid> project).
--
-- Each 'SeqNode' carries a couple bits of information, determining which
-- depth (0 or 2) is in effect, and whether to spark parallel evaluation
-- when the depth is 2.  This state can be configured statically
-- or dynamically.

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Seqable
  (

      rnf_
    , force_
    , seq_

    , SeqNode(..)  -- re-export

-- Must await seqaid implementation:
#if 0
    , mkSeqableHarness
--  , initSeqableHarness
#endif

  )
  where

-------------------------------------------------------------------------------

#if USE_PAR_SEQABLE
  import Control.Parallel ( par )
#endif

--import Data.Generics ( GenericT, mkT, everywhere )

  import Control.DeepSeq.Bounded.NFDataN
  import Control.DeepSeq.Bounded.Pattern

--import Data.Typeable ( Typeable )
--import Data.Data ( Data )

#if JUST_ALIAS_GSEQABLE && ! HASKELL98_FRAGMENT
  import Control.DeepSeq.Bounded.Generic.GSeqable
  import Generics.SOP ( Generic )
#else
  import Control.DeepSeq.Bounded.Pattern ( SeqNode(..) )
#endif

-------------------------------------------------------------------------------

-- Had to move (to Pattern.hs), due to GHC ongoing restrictions
-- making cyclical imports nearly impossible, even if the
-- dependency graph, of exports actually used, is acyclic.
#if 0
  -- Note that Ord is derived, so the order that the constructors
  -- are listed matters!  (This only affects GHC rules, SFAIK.)
  data SeqNode =
           Insulate
         | Propagate
         | Spark
    deriving ( Eq, Ord )
#endif

-------------------------------------------------------------------------------

-- infixr 0 $!!

-------------------------------------------------------------------------------

#if JUST_ALIAS_GSEQABLE && ! HASKELL98_FRAGMENT
#if 1
  rnf_ :: forall a. Generic a => SeqNode -> a -> ()
  rnf_ = grnf_
  force_ :: forall a. Generic a => SeqNode -> a -> a
  force_ = gforce_
  seq_ :: forall a b. Generic a => SeqNode -> a -> b -> b
  seq_ = gseq_
#else
  rnf_ = grnf_ :: forall a. Generic a => SeqNode -> a -> ()
  force_ = gforce_ :: forall a. Generic a => SeqNode -> a -> a
  seq_ = gseq_ :: forall a b. Generic a => SeqNode -> a -> b -> b
#endif
#else

-------------------------------------------------------------------------------

  rnf_ :: NFDataN a => SeqNode -> a -> ()
  rnf_ Insulate     a  =                ()
#if USE_PAR_SEQABLE
  rnf_ Propagate    a  = rnfn 2 a `seq` ()
  rnf_ {-Spark-}_   a  = rnfn 2 a `par` ()
#else
  rnf_ {-Propagate-}_ a  = rnfn 2 a `seq` ()
#endif

-------------------------------------------------------------------------------

  force_ :: NFDataN a => SeqNode -> a -> a
  force_ Insulate     a  =                a
#if USE_PAR_SEQABLE
  force_ Propagate    a  = rnfn 2 a `seq` a
  force_ {-Spark-}_   a  = rnfn 2 a `par` a
#else
  force_ {-Propagate-}_ a  = rnfn 2 a `seq` a
#endif

-------------------------------------------------------------------------------

  seq_ :: NFDataN a => SeqNode -> a -> b -> b
  seq_ Insulate     a b  =                b
#if USE_PAR_SEQABLE
  seq_ Propagate    a b  = rnfn 2 a `seq` b
  seq_ {-Spark-}_   a b  = rnfn 2 a `par` b
#else
  seq_ {-Propagate-}_ a b  = rnfn 2 a `seq` b
#endif

-------------------------------------------------------------------------------

#endif

