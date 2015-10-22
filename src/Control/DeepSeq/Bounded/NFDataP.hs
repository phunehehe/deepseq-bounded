
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#define DO_TRACE 0

#define HANDLE_ATTRS_DATA_CONSTRAINT 0

-- XXX Note: Show constraints are for debugging purposes.
#define INCLUDE_SHOW_INSTANCES 0

-- Formerly DEBUG_WITH_DEEPSEQ_GENERICS.
-- Now also needed to force issuance of all compilePat warnings
-- (so not strictly a debugging flag anymore).
-- [Except it didn't work...]
--- #define NFDATA_INSTANCE_PATTERN 1  -- now it's a .cabal flag

-- Now specified via --flag=[-]USE_WWW_DEEPSEQ
--- #define USE_WW_DEEPSEQ 1

-------------------------------------------------------------------------------

  -- Used to create a custom Exception instance -- needed?
  -- I know we are no longer allowed to write our own instance?
  -- I thought Exceptions were in Haskell 98?...
  {-# LANGUAGE DeriveDataTypeable #-}

  -- For tracing only:
  {-  LANGUAGE BangPatterns #-}
  {-# LANGUAGE BangPatterns #-}

  {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
  {-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

#if ! HASKELL98_FRAGMENT
  {-# LANGUAGE ScopedTypeVariables #-}
  {-  LANGUAGE RankNTypes #-}
  {-# LANGUAGE ConstraintKinds #-}
#endif

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.NFDataP
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides an overloaded function, 'deepseqp', for partially
-- (or fully) evaluating data structures to bounded depth via pattern
-- matching on term shape, and on class, type, and constructor names.
--
-- There are two ways to use this API.
--
--  (1) You can use the 'PatNode' constructors directly.
--
--  (2) You can compile your patterns from strings in a concise
--      embedded language.
--
-- There's no difference in expressive power, but use of the DSL
-- is recommended, because the embedded 'Pattern' compiler can catch
-- some errors that GHC cannot (using 'PatNode' constructors explicitly).
-- Also, the pattern strings are easier to read and write.
--
-- With some qualifications (concerning 'WI' nodes, and 'PatNodeAttrs'
-- configuration), composition fuses, and what's more, it's commutative...
--
-- __Motivation__
--
-- A typical use is to ensure any exceptions hidden within lazy
-- fields of a data structure do not leak outside the scope of the
-- exception handler; another is to force evaluation of a data structure in
-- one thread, before passing it to another thread (preventing work moving
-- to the wrong threads). Unlike <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html DeepSeq>, potentially infinite values of coinductive
-- data types are supported by principled bounding of deep evaluation.
--
-- It is also useful for diagnostic purposes when trying to understand
-- and manipulate space\/time trade-offs in lazy code,
-- and as an optimal substitute for 'deepseq'
-- (where \"optimal\" doesn't include changing the code to remove
-- the need for artificial forcing!).
--
-- 'deepseqp' with optimal patterns is usually a better solution
-- even than stict fields in your data structures, because the
-- latter will behave strictly everywhere the constructors
-- are used, instead of just where its laziness is problematic.
--
-- There may be possible applications to the prevention of resource leaks
-- in lazy streaming, but I'm not certain.
--
-- __Semantics__
--
-- (For additional details, see "Control.DeepSeq.Bounded.Pattern".)
--
-- 'deepseqp' and friends artifically force evaluation of a term
-- so long as the pattern matches.
--
-- A mismatch occurs at a pattern node when the corresponding constructor node either:
--
--  * has arity different than the number of subpatterns (only when subpatterns given)
--
--  * has class\/type\/name not named in the constraint (only when constraint given)
--
-- A mismatch will cause evaluation down that branch to stop, but any
-- upstream matching/forcing will continue uninterrupted.
-- / (This behaviour can now be changed with 'PatNodeAttrs', available since 0.6.) /
--
-- Note that patterns may extend beyond the values they match against,
-- without incurring any mismatch. This semantics is not the only
-- possible, but bear in mind that order of evaluation is nondeterministic,
-- barring further measures.
-- / (This behaviour can also now be changed with 'PatNodeAttrs'.) /
--
-- See also <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataPDyn.html NFDataPDyn> for another approach, which dynamically
-- generates forcing patterns, and can depend on value info
-- (in addition to type info).
-- / (These dynamic aspects never received the attention I intended to give them, I got so caught up in seqaid, which offers similar features. Hopefully actual use of these tools in the near future will give me some perspective on whether/
-- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataPDyn.html NFDataPDyn> /should get attention.) /
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.NFDataP
  (

#if OVERLOADED_STRINGS

       deepseqp, forcep    -- take Pattern arg or String **literal**
     , deepseqp_, forcep_  -- aliases to the aforegoing (for compatability)

#else

     -- * Pattern-bounded analogues of 'deepseq' and 'force'

       deepseqp, forcep    -- take String arg (pattern DSL)

     -- * Avoid DSL compilation overhead
     --
     -- However, we don't anticipate that this overhead would be
     -- significant in most applications, because using <deepseq-bounded>
     -- in a tight loop would only be done for diagnostic purposes.

     , deepseqp_, forcep_  -- take Pattern structure arg

#endif

     -- * A custom exception, raised by choice 'PatNode's, that can be caught in the caller

#if USE_PING_PATNODE
     , DeepSeqBounded_PingException(..)
#endif

     -- * Related modules re-exported

     , module Control.DeepSeq.Bounded.Pattern
     , module Control.DeepSeq.Bounded.PatUtil  -- actually exports former

     -- * Class of things that can be evaluated over an arbitrary finite pattern

     , NFDataP(..)

     -- * Shared with GNFDataP (internal use)

     , handleAttrs  -- used by GNFDataP at least

  )
  where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded.Pattern
  import Control.DeepSeq.Bounded.Compile
  import Control.DeepSeq.Bounded.PatUtil ( unionPats, liftPats )

-- debugging...
#if 1
  import Control.DeepSeq.Bounded.PatUtil ( 
             probDensRose
           , weightedRose
           , unzipRose
           , showRose
#if ! HASKELL98_FRAGMENT
           , Shape
           , shapeOf
           , ghom
#endif
         )
#endif

  import Control.DeepSeq.Bounded.NFDataN  -- finally used ("*3" etc.)

#if USE_WW_DEEPSEQ
  import Control.DeepSeq ( NFData )
  import Control.DeepSeq ( rnf )
#endif

#if HANDLE_ATTRS_DATA_CONSTRAINT
  -- Brought back only to add Data d constraint to handleAttrs, which
  -- is a hack so can print something distinct for multiple +-nodes...
  import Data.Data
#endif
--import Data.Data  -- "redundant" last checked

  import Data.Typeable ( Typeable )
#if 1
  import Data.Typeable ( typeOf )
#else
-- XXX These are NOT interchangeable!
#if __GLASGOW_HASKELL__ >= 781
  import Data.Typeable ( typeRep )
#else
  import Data.Typeable ( typeOf )
#endif
#endif
  import Data.Typeable ( mkTyCon3, mkTyConApp )
  import Data.Typeable ( typeRepTyCon )

#if USE_PAR_PATNODE
  import Control.Parallel ( par )
#endif
#if USE_PSEQ_PATNODE
  import Control.Parallel ( pseq )
#endif
#if USE_PING_PATNODE
  import Control.Concurrent ( myThreadId, killThread )
  import Control.Concurrent ( forkIO )
#endif

  import Control.Concurrent ( threadDelay )

  import Data.Int
  import Data.Word
  import Data.Ratio
  import Data.Complex
  import Data.Array
  import Data.Fixed
  import Data.Version

  import Data.Maybe ( Maybe(..), isJust, fromJust, isNothing )

  import Control.Exception ( Exception )
  import Control.Exception ( asyncExceptionFromException )
  import Control.Exception ( throwTo )
  import Control.Exception ( throw )
  import Control.Exception( AsyncException( UserInterrupt ) )

  import Control.Monad ( liftM )

  -- XXX unsafePerformIO *is* used [besides indirectly with trace and throwTo].
  -- Grep the source on a case by case basis...
  import System.IO.Unsafe ( unsafeInterleaveIO )
  import System.IO.Unsafe ( unsafeDupablePerformIO )
  import System.IO.Unsafe ( unsafePerformIO )
  import Control.Exception ( evaluate )

  import System.Random ( randomIO )

  import Debug.Trace ( trace )

-------------------------------------------------------------------------------

#if USE_PING_PATNODE
  data DeepSeqBounded_PingException = DeepSeqBounded_PingException String
    deriving (Show, Typeable)

  instance Exception DeepSeqBounded_PingException
#endif

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
#else
  mytrace _ = id
#endif

-------------------------------------------------------------------------------

--infixr 0 $!!

-------------------------------------------------------------------------------

#if OVERLOADED_STRINGS

  {-# DEPRECATED deepseqp_, forcep_ "OverloadedStrings is in effect for pattern strings (since 0.8), so you can use deepseqp and forcep with either a Pattern or a String literal as first argument. To work with String variables, write your own wrapper function calling compilePat (or build with OVERLOEADED_STRINGS flag False). These underscore versions will be removed in 0.9 (mere days from now...)." #-}
  -- For compatibility with prior, non-OVERLOADED_STRINGS API.
  -- I can actually upload this without a major version bump,
  -- even though it very much has the feel of one.  That's
  -- extension magic for ya!
  ------
  -- It seems the signatures are needed, unfortunately.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  deepseqp_ :: NFDataP_ictx a => Pattern -> a -> b -> b
--deepseqp_ :: NFDataP_cctx a => Pattern -> a -> b -> b
#else
#if INCLUDE_SHOW_INSTANCES
  deepseqp_ :: (Show a, NFDataP a) => Pattern -> a -> b -> b
#else
  deepseqp_ :: NFDataP a => Pattern -> a -> b -> b
#endif
#endif
  deepseqp_ = deepseqp
  -- | 'deepseqp_' and 'forcep_' are merely aliases to the
  -- non-underscored functions.  They are vestigial and
  -- retained for compatibility until the next major
  -- version bump.  Then it seems safe to remove them,
  -- since can always build with @OVERLOADED_STRINGS@ flag
  -- set @False@ if their absence is a problem (or define
  -- the aliases yourself). So, DEPRECATED, and slated
  -- for removal in 0.8.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  forcep_ :: NFDataP_ictx a => Pattern -> a -> a
--forcep_ :: NFDataP_cctx a => Pattern -> a -> a
#else
#if INCLUDE_SHOW_INSTANCES
  forcep_ :: (Show a, NFDataP a) => Pattern -> a -> a
#else
  forcep_ :: NFDataP a => Pattern -> a -> a
#endif
#endif
  forcep_ = forcep

-- XXX NOTE TO SELF: These comments are verbatim from Control.DeepSeq:
  -- | 'deepseqp' evaluates the first argument to the extent specified
  -- by a 'Pattern', before returning the second.
  --
  -- Quoting from the <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html DeepSeq> documentation (<http://hackage.haskell.org/package/deepseq deepseq> package):
  --
  -- \"<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:deepseq deepseq> /can be useful for forcing pending exceptions, eradicating space leaks, or forcing lazy I\/O to happen.  It is also useful in conjunction with parallel Strategies (see the/ <http://hackage.haskell.org/package/parallel parallel> /package). /
  --
  -- Self-composition fuses via
  --
  -- @
  --     "deepseqp/composition"
  --        forall p1 p2 x1 x2.
  --            (.) ('deepseqp' p2 x2) ('deepseqp' p1 x1)
  --          = 'deepseqp' ( 'liftPats' [p1, p2] ) (x1,x2)
  -- @
  --
  -- (Other fusion rules, not yet documented, may also be in effect.)
{--}
  -- [No longer true, but discussing doPseq here is too much, so tacit.]
  -- / There is no guarantee about the ordering of evaluation.  The implementation may evaluate the components of the structure in any order or in parallel.  To impose an actual order on evaluation, use 'pseq' from "Control.Parallel" in the @parallel@ package.\" /
{--}
-- XXX LATER: This is flawed for # at root of pattern.
-- Maybe not quite flawed, just "untestable" if there's bottom at
-- the root of the value (that is to say, the value is (undefined::b).
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  deepseqp :: NFDataP_ictx a => Pattern -> a -> b -> b
--deepseqp :: NFDataP_cctx a => Pattern -> a -> b -> b
#else
#if INCLUDE_SHOW_INSTANCES
  deepseqp :: (Show a, NFDataP a) => Pattern -> a -> b -> b
#else
  deepseqp :: NFDataP a => Pattern -> a -> b -> b
#endif
#endif
  deepseqp pat a b = rnfp pat a `seq` b
  -- XXX Need to double-check that this makes sense; didn't think
  -- it through -- when b != a, things are not so simple.
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-# RULES
    "deepseqp/composition"    forall p1 p2 x1 x2.  (.) (deepseqp p2 x2) (deepseqp p1 x1) = deepseqp ( liftPats [p1, p2] ) (x1,x2)
      #-}
--  "deepseqp/composition"    forall p1 p2 x.  (.) (deepseqp p2) (deepseqp p1) x = deepseqp ( unionPats [p1, p2] ) x

-------------------------------------------------------------------------------

#if 0
  -- | the deep analogue of '$!'.  In the expression @f $!! x@, @x@ is
  -- fully evaluated before the function @f@ is applied to it.
  ($!!) :: (NFData a) => (a -> b) -> a -> b
  f $!! x = x `deepseq` f x
#endif

-- XXX NOTE TO SELF: These comments are verbatim from Control.DeepSeq:
  -- | A variant of 'deepseqp' that is sometimes convenient:
  --
  -- > forcep pat x = deepseqp pat x x   -- (cannot write x `deepseqp pat` x by analogy with x `deepseq` x)
  --
  -- @forcep pat x@ evaluates @x@ to the depth determined by @pat@, and
  -- then returns @x@.  Again from
  -- <http://hackage.haskell.org/package/deepseq deepseq>:
  -- / \"Note that @forcep pat x@ only takes effect when the value of @forcep pat x@ itself is demanded, so essentially it turns shallow evaluation into evaluation to arbitrary bounded depth.\" /
  --
  -- Self-composition fuses via
  --
  -- @
  --     "forcep/composition"
  --        forall p1 p2 x.
  --            (.) ('forcep' p2) ('forcep' p1) x
  --          = 'forcep' ( 'unionPats' [p1, p2] ) x
  -- @
  --
  -- (Other fusion rules, not yet documented, may also be in effect.)
{--}
-- XXX What about fusion of mixed applications?...
-- XXX LATER: This is "flawed" for # at root of pattern. (See deepseqp.)
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  forcep :: NFDataP_ictx a => Pattern -> a -> a
--forcep :: NFDataP_cctx a => Pattern -> a -> a
#else
#if INCLUDE_SHOW_INSTANCES
  forcep :: (Show a, NFDataP a) => Pattern -> a -> a
#else
  forcep :: NFDataP a => Pattern -> a -> a
#endif
#endif
  forcep pat x = deepseqp pat x x
  {-# RULES
    "forcep/composition"    forall p1 p2 x.  (.) (forcep p2) (forcep p1) x = forcep ( unionPats [p1, p2] ) x
      #-}

#else
-- else not OVERLOADED_STRINGS

-- XXX NOTE TO SELF: These comments are verbatim from Control.DeepSeq:
  -- | 'deepseqp' evaluates the first argument to the extent specified
  -- by a 'Pattern', before returning the second.
  --
  -- Quoting from the <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html DeepSeq> documentation (<http://hackage.haskell.org/package/deepseq deepseq> package):
  --
  -- \"<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:deepseq deepseq> /can be useful for forcing pending exceptions, eradicating space leaks, or forcing lazy I\/O to happen.  It is also useful in conjunction with parallel Strategies (see the/ <http://hackage.haskell.org/package/parallel parallel> /package). /
  --
  -- Composition fuses (see 'deepseqp_').
{--}
  -- [No longer true, but discussing doPseq here is too much, so tacit.]
  -- / There is no guarantee about the ordering of evaluation.  The implementation may evaluate the components of the structure in any order or in parallel.  To impose an actual order on evaluation, use 'pseq' from "Control.Parallel" in the @parallel@ package.\" /
{--}
  -- XXX LATER: This is "flawed" for # at root of pattern. (See deepseqp_.)
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  deepseqp :: NFDataP_ictx a => String -> a -> b -> b
--deepseqp :: NFDataP_cctx a => String -> a -> b -> b
#else
#if INCLUDE_SHOW_INSTANCES
  deepseqp :: (Show a, NFDataP a) => String -> a -> b -> b
#else
  deepseqp :: NFDataP a => String -> a -> b -> b
#endif
#endif
#if 0
#elif 0
  deepseqp patstr a b = fromJust $ deepseqp_ (compilePat patstr) a b
--deepseqp patstr = fromJust $ deepseqp_ (compilePat patstr)
#elif 1
  deepseqp patstr = deepseqp_ (compilePat patstr)
#elif 0
  deepseqp patstr a b = rnfp (compilePat patstr) a `seq` b
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-# RULES
    "deepseqp/composition"    forall p1 p2 x.  (.) (deepseqp p2) (deepseqp p1) x = deepseqp_ ( unionPats [compilePat p1, compilePat p2] ) x
      #-}
#endif

  -- | Self-composition fuses via
  --
  -- @
  --     "deepseqp_/composition"
  --        forall p1 p2 x1 x2.
  --            (.) ('deepseqp_' p2 x2) ('deepseqp_' p1 x1)
  --          = 'deepseqp_' ( 'liftPats' [p1, p2] ) (x1,x2)
  -- @
  --
  -- (Other fusion rules, not yet documented, may also be in effect.)
{--}
-- XXX LATER: This is "flawed" for # at root of pattern.
-- Maybe not quite "flawed", just "untestable" if there's bottom at
-- the root of the value (that is to say, the value is (undefined::b).
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  deepseqp_ :: NFDataP_ictx a => Pattern -> a -> b -> b
--deepseqp_ :: NFDataP_cctx a => Pattern -> a -> b -> b
#else
#if INCLUDE_SHOW_INSTANCES
  deepseqp_ :: (Show a, NFDataP a) => Pattern -> a -> b -> b
#else
  deepseqp_ :: NFDataP a => Pattern -> a -> b -> b
#endif
#endif
#if 0
#elif 0
  deepseqp_ pat@(Node WI _) _ b = b
  deepseqp_ pat@(Node (TR as) chs) a b = if elem ta treps then doit `seq` b else b
   where ta = show $ typeRepTyCon $ typeOf a
         doit = rnfp pat a `seq` b
         treps = typeConstraints as
  deepseqp_ pat@(Node (TI as) chs) a b = if elem ta treps then b else doit `seq` b
   where ta = show $ typeRepTyCon $ typeOf a
         doit = rnfp pat a `seq` b
         treps = typeConstraints as
  deepseqp_ pat a b = rnfp pat a `seq` b
#elif 1
  deepseqp_ pat a b = rnfp pat a `seq` b
  -- XXX Need to double-check that this makes sense; didn't think
  -- it through -- when b != a, things are not so simple.
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-# RULES
    "deepseqp_/composition"    forall p1 p2 x1 x2.  (.) (deepseqp_ p2 x2) (deepseqp_ p1 x1) = deepseqp_ ( liftPats [p1, p2] ) (x1,x2)
      #-}
--  "deepseqp_/composition"    forall p1 p2 x.  (.) (deepseqp_ p2) (deepseqp_ p1) x = deepseqp_ ( unionPats [p1, p2] ) x
#endif

-------------------------------------------------------------------------------

#if 0
  -- | the deep analogue of '$!'.  In the expression @f $!! x@, @x@ is
  -- fully evaluated before the function @f@ is applied to it.
  ($!!) :: (NFData a) => (a -> b) -> a -> b
  f $!! x = x `deepseq` f x
#endif

-- XXX NOTE TO SELF: These comments are verbatim from Control.DeepSeq:
  -- | A variant of 'deepseqp' that is sometimes convenient:
  --
  -- > forcep pat x = deepseqp pat x x   -- (cannot write x `deepseqp pat` x by analogy with x `deepseq` x)
  --
  -- @forcep pat x@ evaluates @x@ to the depth determined by @pat@, and
  -- then returns @x@.  Again from
  -- <http://hackage.haskell.org/package/deepseq deepseq>:
  -- / \"Note that @forcep pat x@ only takes effect when the value of @forcep pat x@ itself is demanded, so essentially it turns shallow evaluation into evaluation to arbitrary bounded depth.\" /
  --
  -- Composition fuses (see 'forcep_').
{--}
-- XXX What about fusion of mixed applications?...
-- XXX LATER: This is "flawed" for # at root of pattern. (See deepseqp_.)
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  forcep :: NFDataP_ictx a => String -> a -> a
--forcep :: NFDataP_cctx a => String -> a -> a
#else
#if INCLUDE_SHOW_INSTANCES
  forcep :: (Show a, NFDataP a) => String -> a -> a
#else
  forcep :: NFDataP a => String -> a -> a
#endif
#endif
#if 0
#elif 0
  forcep patstr x
   | p          = fromJust ma
-- | otherwise  = error "here"
   | otherwise  = undefined::a
   where ma = deepseqp_ (compilePat patstr) x x
         p = isJust ma
#elif 0
  forcep patstr x = fromJust $ deepseqp_ (compilePat patstr) x x
#elif 0
  forcep patstr x
   | b          = x
   | otherwise  = fromJust y
   where y = deepseqp_ (compilePat patstr) x (Just x)
         pat@(Node pas chs) = compilePat patstr
         b | WI <- pas  = True
           | TR <- pas  = True
           | TN <- pas  = True
           | TW <- pas  = True
           | TI <- pas  = True
           | otherwise  = False
#elif 1
  forcep patstr x = deepseqp_ (compilePat patstr) x x
--forcep patstr x = deepseqp patstr x x
  {-# RULES
    "forcep/composition"    forall p1 p2 x.  (.) (forcep p2) (forcep p1) x = forcep_ ( unionPats [compilePat p1, compilePat p2] ) x
      #-}
#endif

  -- | Self-composition fuses via
  --
  -- @
  --     "forcep_/composition"
  --        forall p1 p2 x.
  --            (.) ('forcep_' p2) ('forcep_' p1) x
  --          = 'forcep_' ( 'unionPats' [p1, p2] ) x
  -- @
  --
  -- (Other fusion rules, not yet documented, may also be in effect.)
{--}
-- XXX LATER: This is "flawed" for # at root of pattern. (See deepseqp_.)
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  forcep_ :: NFDataP_ictx a => Pattern -> a -> a
--forcep_ :: NFDataP_cctx a => Pattern -> a -> a
#else
#if INCLUDE_SHOW_INSTANCES
  forcep_ :: (Show a, NFDataP a) => Pattern -> a -> a
#else
  forcep_ :: NFDataP a => Pattern -> a -> a
#endif
#endif
#if 0
  forcep_ pat x = fromJust $ deepseqp_ pat x x
#else
  forcep_ pat x = deepseqp_ pat x x
  {-# RULES
    "forcep_/composition"    forall p1 p2 x.  (.) (forcep_ p2) (forcep_ p1) x = forcep_ ( unionPats [p1, p2] ) x
      #-}
#endif

#endif
-- of if OVERLOADED_STRINGS else

-------------------------------------------------------------------------------

#if ! HASKELL98_FRAGMENT
-- We don't need 7.10 for tuple predicates /here/, but when try to
-- use in seqaid, the TH splice complains
--   Can't represent tuple predicates in Template Haskell:
--     Control.DeepSeq.Bounded.NFDataP_new_grammar.NFDataP_cctx
-- That is with GHC 7.8.4.
-- So we need template-haskell-2.10, which means need base >= base-4.8.0.0,
-- which means need GHC >= 7.10.
#if __GLASGOW_HASKELL__ >= 710
  -- XXX Not H98! -XConstraintKinds
  type NFDataP_cctx a
        = (
              Typeable a
#if HANDLE_ATTRS_DATA_CONSTRAINT
            , Data a
#endif
            , NFDataN a
#if USE_WW_DEEPSEQ
            , NFData a
#endif
          )
#if 0
-- XXX Because TH < 2.10 cannot hand tuple predicates,
-- even if the corresponding GHC can!
-- And this didn't help anyway, still get the error
-- when TH runs in seqaid...
  type NFDataP_ictx a
        = (
              Typeable a
#if HANDLE_ATTRS_DATA_CONSTRAINT
            , Data a
#endif
            , NFDataN a
#if USE_WW_DEEPSEQ
            , NFData a
#endif
            , NFDataP a
#if INCLUDE_SHOW_INSTANCES
            , Show a
#endif
          )
#else
  type NFDataP_ictx a
        = (
              NFDataP_cctx a
            , NFDataP a
#if INCLUDE_SHOW_INSTANCES
            , Show a
#endif
          )
#endif
#endif
#endif

-------------------------------------------------------------------------------

  -- | A class of types that can be evaluated over an arbitrary finite pattern.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  class NFDataP_cctx a => NFDataP a where
#else
  class (
          Typeable a
#if HANDLE_ATTRS_DATA_CONSTRAINT
        , Data a
#endif
        , NFDataN a
#if USE_WW_DEEPSEQ
        , NFData a
#endif
        ) => NFDataP a where
#endif
    -- | Self-composition fuses via
    --
    -- @
    --     "rnfp/composition"
    --        forall p1 p2 x.
    --            (.) ('rnfp' p2) ('rnfp' p1) x
    --          = 'rnfp' ( 'unionPats' [p1, p2] ) x
    -- @
    --
    -- (Other fusion rules, not yet documented, may also be in effect.)
    {-  NOINLINE rnfp #-}
#if INCLUDE_SHOW_INSTANCES
    rnfp :: Show a => Pattern -> a -> ()
#else
    rnfp :: Pattern -> a -> ()
#endif
#if 0
--  rnfp p x | p `seq` trace "Boo!" () `seq` p == Node XX []  = undefined  -- does NOT work
--  rnfp p x | trace "Boo!" p `seq` p == Node XX []  = undefined  -- WORKS!!!
--  rnfp p x | trace "Boo!" () `seq` p == (trace "FOO!!" (Node XX []))  = undefined  -- FOO!! /is/ printed, though only once (like Boo)
--  rnfp p x | trace "Boo!" () `seq` p == Node XX []  = undefined  -- NOT works (printed once only)
--  rnfp p x | (trace "Boo!" p) == Node XX []  = undefined  -- WORKS!!! (printed every time)
--  rnfp p x | trace "Boo!" () `seq` p == Node TN{} []  = undefined
--  rnfp p x | trace "Boo!" () `seq` False  = undefined
--  rnfp p x | trace "Boo!" True, p `seq` False  = undefined
--  rnfp p x | trace "Boo!" False  = undefined
#endif
#if 1
    rnfp p x | handleAttrs p x == Node XX []  = undefined
--  rnfp p@(Node pn _) x | as <- getPatNodeAttrs pn, 9 == uniqueID as  = error $ showPatNodeRaw pn
--  rnfp p@(Node pn@(WI _) _) x = error $ showPatNodeRaw pn
--  rnfp p@(Node pn@WI{} _) x = error $ showPatNodeRaw pn
--  rnfp p@(Node WI{} _) x = handleAttrs p x `seq` ()
    rnfp (Node WI{} _) _ = ()
#else
    rnfp (Node WI{} _) _ = ()
#endif
    rnfp (Node (TR as) chs) d = if elem td treps then d `seq` () else ()
     where td = show $ typeRepTyCon $ typeOf d
           treps = typeConstraints as
    rnfp (Node (TI as) chs) d = if elem td treps then () else d `seq` ()
     where td = show $ typeRepTyCon $ typeOf d
           treps = typeConstraints as
#if USE_WW_DEEPSEQ
    -- complement of TI, equivalent to TR in this special case
    rnfp (Node (TW as) chs) d = if elem td treps then d `seq` () else ()
     where td = show $ typeRepTyCon $ typeOf d
           treps = typeConstraints as
#endif
#if 1
    rnfp _ d = d `seq` ()
--  rnfp _ _ = ()
#else
    -- XXX temporarily (at least) commenting out; not making any
    -- use of patternShapeOK -- but there is room for more
    -- error-trapping code in here, definitely...
    rnfp pat a | not $ patternShapeOK pat a  = ()
               | otherwise  = rnf a
#endif

  {-# RULES
    "rnfp/composition"    forall p1 p2 x.  (.) (rnfp p2) (rnfp p1) x = rnfp ( unionPats [p1, p2] ) x
      #-}
--  "rnfp/composition"    forall p1 p2 x.  compose (rnfp p2) (rnfp p1) x = rnfp ( unionPats [p1, p2] ) x
--  "rnfp/composition"    forall p1 p2 x.  ( rnfp p2 . rnfp p1 ) x = rnfp ( unionPats [p1, p2] ) x

-------------------------------------------------------------------------------

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  rnfp' :: NFDataP_ictx a => PatNode -> () -> a -> ()
#else
#if USE_WW_DEEPSEQ
  rnfp' :: (Typeable a, NFDataN a, NFData a) => PatNode -> () -> a -> ()
#else
  rnfp' :: (Typeable a, NFDataN a) => PatNode -> () -> a -> ()
#endif
#endif
  rnfp' pas recurs d
   -- I can only conclude that we've already evaluated the argument
   -- by the time this code runs (or that we evaluate it after...).
   -- But this code doesn't do the dirty deed!
   = {-trace ( "rnfp' " ++ show pat) $-}
     let
         td = show $ typeRepTyCon $ typeOf d  -- no problem on bottom
         as = getPatNodeAttrs pas
         treps = typeConstraints as
         n = depth as
     in
#if USE_PAR_PATNODE
     if doSpark as
     then
       case pas of
        WS{} -> () `par` ()
        WR{} -> recurs `par` ()
        WN{} -> rnfn n d `par` ()
#if USE_WW_DEEPSEQ
        WW{} -> rnf d `par` ()  -- should reimplement deepseq for uniformity...
#endif
        WI{} -> error "rnfp: unexpected =WI (please report this bug!)"
        _ -> error $ "rnfp: Unexpected PatNode (with doSpark): " ++ show pas ++ "(please report this bug!)"
     else
#endif
       case pas of
        WR{} -> recurs
        WS{} -> ()
--      WS{} -> d `seq` ()
        WN{} -> rnfn n d
#if USE_WW_DEEPSEQ
        WW{} -> rnf d  -- should reimplement deepseq for uniformity...
#endif
#if 0
        -- This code stays the same whether we (are able to) compare
        -- actual TypeRep's for equality, or we just hack it
        -- and match:  show . typeRepTyCon . typeOf
        TR{} ->  if       elem td treps then recurs else ()
#endif
#if 0
        -- This is not right. To pull this off (b/c it depends on
        -- types in the value you're matching with), need to construct
        -- the pattern dynamically.  In particular, to produce Nil
        -- where a TR (or TS) constraint causes (or would cause)
        -- match failure.
        TS{} ->  if       elem td treps then () else ()
#endif
        TN{} ->  if       elem td treps then rnfn n d else ()
#if USE_WW_DEEPSEQ
        TW{} ->  if       elem td treps then rnf d else ()
#endif
#if 0
        NTR{} -> if not $ elem td treps then recurs else ()
        NTN{} -> if not $ elem td treps then rnfn n d else ()
#if USE_WW_DEEPSEQ
        NTW{} -> if not $ elem td treps then rnf d else ()
#endif
#endif
        -- these handled upstream
        WI{} -> error "rnfp: unexpected WI (please report this bug!)"
        TR{} -> error "rnfp: unexpected TR (please report this bug!)"
        TI{} -> error "rnfp: unexpected TI (please report this bug!)"
        _ -> error $ "rnfp: Unexpected PatNode: " ++ show pas ++ " (please report this bug!)"

-------------------------------------------------------------------------------

#if 0
  compose = (.)
  {-# NOINLINE compose #-}
  -- Can't do this, unfortunately.  GHC warns it may get inlined before
  -- rules have a chance to fire.  I would rather avoid forcing the API
  -- user to use some custom "compose" function, since base (.) works
  -- perfectly, except it's hard to control its inlining...
  {-  NOINLINE (.) #-}
#endif

-------------------------------------------------------------------------------
-- Base nullary types.

  instance NFDataP Int
  instance NFDataP Word
  instance NFDataP Integer
  instance NFDataP Float
  instance NFDataP Double

  instance NFDataP Char
  instance NFDataP Bool
  instance NFDataP ()

  instance NFDataP Int8
  instance NFDataP Int16
  instance NFDataP Int32
  instance NFDataP Int64

  instance NFDataP Word8
  instance NFDataP Word16
  instance NFDataP Word32
  instance NFDataP Word64

-------------------------------------------------------------------------------
--- Fixed a

  instance Typeable a => NFDataP (Fixed a)
--instance NFDataP (Fixed a)

-------------------------------------------------------------------------------
--- a -> b

  -- [Quoted from deepseq:]
  -- This instance is for convenience and consistency with 'seq'.
  -- This assumes that WHNF is equivalent to NF for functions.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b) => NFDataP (a -> b)
#else
  instance (
             Typeable a, Typeable b
#if HANDLE_ATTRS_DATA_CONSTRAINT
           , Data a, Data b  -- XXX why only needed in THIS instance?
#endif
           ) => NFDataP (a -> b)
--instance NFDataP (a -> b)
#endif

-------------------------------------------------------------------------------
--- Ratio a

  -- not taken to be a level of depth
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, Integral a) => NFDataP (Ratio a) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Integral a, NFDataP a) => NFDataP (Ratio a) where
#else
  instance (Integral a, NFDataP a) => NFDataP (Ratio a) where
#endif
#endif
    -- XXX This is very dubious!...
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat x = rnfp pat (numerator x, denominator x)

-------------------------------------------------------------------------------
--- Complex a

  -- Note that (Complex a) constructor (:+) has strict fields,
  -- so unwrapping the ctor also forces both components.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, RealFloat a) => NFDataP (Complex a) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, RealFloat a, NFDataP a) => NFDataP (Complex a) where
#else
  instance (RealFloat a, NFDataP a) => NFDataP (Complex a) where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        2 -> let [px,py] = chs
                 (x:+y) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px x
                                   , rnfp py y
                                   ]
#else
             in       rnfp px x
                `seq` rnfp py y
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(Complex a)" pas chs d

-------------------------------------------------------------------------------
--- Maybe a

-- XXX Never until now (so much later) did I properly consider how to
-- handle a no-arg (nullary) constructor! It's not hard, but it's
-- significantly different than the other cases, as there's no
-- subvalue we can grab hold of -- there's no "d"; so this invalidates
-- all the code, in the posary (complement of nullary, coinage :) case
-- case, which references d.
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance NFDataP_ictx a => NFDataP (Maybe a) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, NFDataP a) => NFDataP (Maybe a) where
#else
  instance NFDataP a => NFDataP (Maybe a) where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) Nothing
     | not $ null chs  = pat_match_fail
     | otherwise       = ()
     where
      pat_match_fail = patMatchFail' "Nothing" pas chs ()
    rnfp (Node pas chs) (Just d)
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        1 -> let [p_J] = chs
             in       rnfp p_J d
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "Just" pas chs d
    rnfp (Node pas chs) d = patMatchFail pas chs d  -- unreachable

-------------------------------------------------------------------------------
--- Either a b

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b) => NFDataP (Either a b) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Show b, NFDataP a, NFDataP b) => NFDataP (Either a b) where
#else
  instance (NFDataP a, NFDataP b) => NFDataP (Either a b) where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp (Node pas chs) (Left d)
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        1 -> let [p_L] = chs
             in       rnfp p_L d
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "Left" pas chs d
--    pat_match_fail = patMatchFail' "(Either a b)" pas chs d
    rnfp (Node pas chs) (Right d)
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise     = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        1 -> let [p_R] = chs
             in       rnfp p_R d
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "Right" pas chs d
--    pat_match_fail = patMatchFail' "(Either a b)" pas chs d
    rnfp (Node pas chs) d = patMatchFail pas chs d  -- unreachable

-------------------------------------------------------------------------------
--- Data.Version.Version

--- #if __GLASGOW_HASKELL__ < 781
--- -- requires -XStandaloneDeriving
--- -- orphan instance, but better than dropping support
--- -- (It seems it already has its Show instance!)
--- deriving instance Data Data.Version.Version
--- #endif

--deriving instance Data TypeRep  -- can't b/c not all data ctors are in scope!

  -- Data.Version ctor does /not/ have strict fields.
  instance NFDataP Data.Version.Version where
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        2 -> let [pbr,ptags] = chs
                 Data.Version.Version branch tags = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [rnfp pbr branch, rnfp ptags tags]
#else
             in       rnfp pbr branch
                `seq` rnfp ptags tags
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "Data.Version.Version" pas chs d

-------------------------------------------------------------------------------
--- [a]

  -- Data.List ctors do /not/ have strict fields (i.e. (:) is not strict).
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance NFDataP_ictx a => NFDataP [a] where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, NFDataP a) => NFDataP [a] where
#else
  instance NFDataP a => NFDataP [a] where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp _ [] = ()  -- perhaps dubious?...
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        2 -> let [px,pxs] = chs
                 (x:xs) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [rnfp px x, rnfp pxs xs]
#else
             in       rnfp px x
                `seq` rnfp pxs xs
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "[a]" pas chs d

-------------------------------------------------------------------------------
--- Array a b

  -- Data.Array ctor does /not/ have strict fields.
  -- not taken to be a level of depth
#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (Ix a, NFDataP_ictx a, NFDataP_ictx b) => NFDataP (Array a b) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Show b, Ix a, NFDataP a, NFDataP b) => NFDataP (Array a b) where
#else
  instance (Ix a, NFDataP a, NFDataP b) => NFDataP (Array a b) where
#endif
#endif
    -- XXX This is very dubious!...
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pas x =        rnfp pas (bounds x, Data.Array.elems x)
                  `seq` ()  -- needed?

-------------------------------------------------------------------------------
--- (a,b)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b) => NFDataP (a,b) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a,Typeable a,NFDataP a, Show b,Typeable b,NFDataP b) => NFDataP (a,b) where
#else
  instance (Typeable a,NFDataP a, Typeable b,NFDataP b) => NFDataP (a,b) where
#endif
#endif
    {-  NOINLINE rnfp #-}
--  rnfp _ _ = error "booy"
--  rnfp p x | trace "hoop!" (handleAttrs p x) `seq` p == Node XX []  = undefined
    rnfp p x | handleAttrs p x `seq` p == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        2 -> let [px,py] = chs
                 (x,y) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [rnfp px x, rnfp py y]
#else
             in       rnfp px x
                `seq` rnfp py y
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,)" pas chs d

-------------------------------------------------------------------------------
--- (a,b,c)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c) => NFDataP (a,b,c) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c) => NFDataP (a,b,c) where
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c) => NFDataP (a,b,c) where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        3 -> {-trace "WWW" $-}
             let [px,py,pz] = chs
                 (x,y,z) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px x
                                   , rnfp py y
                                   , rnfp pz z
                                   ]
#else
             in       ({-trace "XXX" $-} rnfp px x)
                `seq` ({-trace "YYY" $-} rnfp py y)
-- This WILL change the semantics unfortunately...
--              `seq` (trace ("YYY "++show py++" "++show y) $ rnfp py y)
                `seq` ({-trace "ZZZ" $-} rnfp pz z)
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,)" pas chs d

-------------------------------------------------------------------------------
--- (a,b,c,d)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c, NFDataP_ictx d) => NFDataP (a,b,c,d) where
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c, Show d, Typeable d, NFDataP d) => NFDataP (a,b,c,d) where
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c, Typeable d, NFDataP d) => NFDataP (a,b,c,d) where
#endif
#endif
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        4 -> let [px1,px2,px3,px4] = chs
                 (x1,x2,x3,x4) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px1 x1
                                   , rnfp px2 x2
                                   , rnfp px3 x3
                                   , rnfp px4 x4
                                   ]
#else
             in       rnfp px1 x1
                `seq` rnfp px2 x2
                `seq` rnfp px3 x3
                `seq` rnfp px4 x4
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,,)" pas chs d

-------------------------------------------------------------------------------
--- (a,b,c,d,e)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c, NFDataP_ictx d, NFDataP_ictx e) =>
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c, Show d, Typeable d, NFDataP d, Show e, Typeable e, NFDataP e) =>
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c, Typeable d, NFDataP d, Typeable e, NFDataP e) =>
#endif
#endif
         NFDataP (a, b, c, d, e) where
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        5 -> let [px1,px2,px3,px4,px5] = chs
                 (x1,x2,x3,x4,x5) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px1 x1
                                   , rnfp px2 x2
                                   , rnfp px3 x3
                                   , rnfp px4 x4
                                   , rnfp px5 x5
                                   ]
#else
             in       rnfp px1 x1
                `seq` rnfp px2 x2
                `seq` rnfp px3 x3
                `seq` rnfp px4 x4
                `seq` rnfp px5 x5
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,,,)" pas chs d

-------------------------------------------------------------------------------
--- (a,b,c,d,e,f)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c, NFDataP_ictx d, NFDataP_ictx e, NFDataP_ictx f) =>
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c, Show d, Typeable d, NFDataP d, Show e, Typeable e, NFDataP e, Show f, Typeable f, NFDataP f) =>
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c, Typeable d, NFDataP d, Typeable e, NFDataP e, Typeable f, NFDataP f) =>
#endif
#endif
         NFDataP (a, b, c, d, e, f) where
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        6 -> let [px1,px2,px3,px4,px5,px6] = chs
                 (x1,x2,x3,x4,x5,x6) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px1 x1
                                   , rnfp px2 x2
                                   , rnfp px3 x3
                                   , rnfp px4 x4
                                   , rnfp px5 x5
                                   , rnfp px6 x6
                                   ]
#else
             in       rnfp px1 x1
                `seq` rnfp px2 x2
                `seq` rnfp px3 x3
                `seq` rnfp px4 x4
                `seq` rnfp px5 x5
                `seq` rnfp px6 x6
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,,,,)" pas chs d

-------------------------------------------------------------------------------
--- (a,b,c,d,e,f,g)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c, NFDataP_ictx d, NFDataP_ictx e, NFDataP_ictx f, NFDataP_ictx g) =>
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c, Show d, Typeable d, NFDataP d, Show e, Typeable e, NFDataP e, Show f, Typeable f, NFDataP f, Show g, Typeable g, NFDataP g) =>
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c, Typeable d, NFDataP d, Typeable e, NFDataP e, Typeable f, NFDataP f, Typeable g, NFDataP g) =>
#endif
#endif
         NFDataP (a, b, c, d, e, f, g) where
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        7 -> let [px1,px2,px3,px4,px5,px6,px7] = chs
                 (x1,x2,x3,x4,x5,x6,x7) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px1 x1
                                   , rnfp px2 x2
                                   , rnfp px3 x3
                                   , rnfp px4 x4
                                   , rnfp px5 x5
                                   , rnfp px6 x6
                                   , rnfp px7 x7
                                   ]
#else
             in       rnfp px1 x1
                `seq` rnfp px2 x2
                `seq` rnfp px3 x3
                `seq` rnfp px4 x4
                `seq` rnfp px5 x5
                `seq` rnfp px6 x6
                `seq` rnfp px7 x7
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,,,,,)" pas chs d

#if 0

-- XXX No Typeable instances for tuples larger than 7 in 7.8.1, seemingly?

-------------------------------------------------------------------------------
--- (a,b,c,d,e,f,g,h)

#if ( ! HASKELL98_FRAGMENT ) && ( __GLASGOW_HASKELL__ >= 710 )
  instance (NFDataP_ictx a, NFDataP_ictx b, NFDataP_ictx c, NFDataP_ictx d, NFDataP_ictx e, NFDataP_ictx f, NFDataP_ictx g, NFDataP_ictx h) =>
#else
#if INCLUDE_SHOW_INSTANCES
  instance (Show a, Typeable a, NFDataP a, Show b, Typeable b, NFDataP b, Show c, Typeable c, NFDataP c, Show d, Typeable d, NFDataP d, Show e, Typeable e, NFDataP e, Show f, Typeable f, NFDataP f, Show g, Typeable g, NFDataP g, Show h, Typeable h, NFDataP h) =>
#else
  instance (Typeable a, NFDataP a, Typeable b, NFDataP b, Typeable c, NFDataP c, Typeable d, NFDataP d, Typeable e, NFDataP e, Typeable f, NFDataP f, Typeable g, NFDataP g, Typeable h, NFDataP h) =>
#endif
#endif
         NFDataP (a, b, c, d, e, f, g, h) where
    {-  NOINLINE rnfp #-}
    rnfp p x | handleAttrs p x == Node XX []  = undefined
    rnfp (Node WI{} _) _ = ()
    rnfp pat@(Node pas chs) d
     | TR{} <- pas  = if elem td treps then recurs else ()
#if USE_WW_DEEPSEQ
     | TI{} <- pas  = if elem td treps then () else rnf d
     | TW{} <- pas  = if elem td treps then rnf d else ()
#else
     | TI{} <- pas  = if elem td treps then () else rnfn 999999 d  -- XXX thack!
#endif
     | otherwise    = rnfp' pas recurs d
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      td = show $ typeRepTyCon $ typeOf d
      recurs = case length chs of
        0 -> case pas of
              WS{} -> ()
              _ -> pat_match_fail
        8 -> let [px1,px2,px3,px4,px5,px6,px7,px8] = chs
                 (x1,x2,x3,x4,x5,x6,x7,x8) = d
#if USE_PSEQ_PATNODE
             in pseq_condition pat [ rnfp px1 x1
                                   , rnfp px2 x2
                                   , rnfp px3 x3
                                   , rnfp px4 x4
                                   , rnfp px5 x5
                                   , rnfp px6 x6
                                   , rnfp px7 x7
                                   , rnfp px8 x8
                                   ]
#else
             in       rnfp px1 x1
                `seq` rnfp px2 x2
                `seq` rnfp px3 x3
                `seq` rnfp px4 x4
                `seq` rnfp px5 x5
                `seq` rnfp px6 x6
                `seq` rnfp px7 x7
                `seq` rnfp px8 x8
#endif
                `seq` ()  -- needed?
        _ -> pat_match_fail
      pat_match_fail = patMatchFail' "(,,,,,,,)" pas chs d

#endif

-------------------------------------------------------------------------------

  patMatchFail :: (Show a, Show b) => a -> b -> c -> ()
  patMatchFail pas chs d
#if WARN_PATTERN_MATCH_FAILURE
   = ( unsafePerformIO $! putStrLn $! "NFDataP: warning: couldn't match " ++ show pas ++ " (having children " ++ show chs ++ ")" ) `seq` ()
#else
   = ()
#endif
-- = error $ "NFDataP: Couldn't match " ++ show pas ++ " (having children " ++ show chs ++ ")\nwith data " ++ show d

  patMatchFail' :: (Show a, Show b) => String -> a -> b -> c -> ()
  patMatchFail' inst pas chs d
#if WARN_PATTERN_MATCH_FAILURE
   = ( unsafePerformIO $! putStrLn $! "NFDataP: warning: instance " ++ inst ++ ": bad PatNode child list" ) `seq` patMatchFail pas chs d
#else
   = ()
#endif

-------------------------------------------------------------------------------

-- XXX Seeing as we're having troubles anyway, and considering
-- that most of this function requires popping into unsafePerformIO
-- anyway -- should the whole thing be returning IO, and then just
-- use unsafePerformIO in caller?

  -- This function collects all the "harmless impure things" we
  -- need to pop into IO to do.  To what extent we get away with
  -- this remains to be investigated...
  ------
  -- So, which of the PatNodeAttrs product type can be dealt with here?
  --   NOT doSpark  - dealt with downstream in rnfp'
  --   NOT doPseq   - about to be dealt with downstream (hopefully rnfp')...
  --       doDelay
  --       doTrace
  --       doPing
  --       doDie
  --       doTiming
  -----
  -- XXX Returning Pattern instead of Bool, in a continued attempt
  -- to outsmart GHC and get certain things to be evaluated...

  {-  NOINLINE handleAttrs #-}
#if 0
-- XXX XXX XXX testing only!!!! XXX XXX XXX
  handleAttrs pat@(Node p _) x = pat
#else
#if ! HASKELL98_FRAGMENT
#if HANDLE_ATTRS_DATA_CONSTRAINT
  handleAttrs :: forall d. Data d => Pattern -> d -> Pattern
--handleAttrs :: forall d. Data d => Pattern -> d -> Bool
  handleAttrs (Node p _) x
#else
  handleAttrs :: forall d. Typeable d => Pattern -> d -> Pattern
--handleAttrs :: forall d. Typeable d => Pattern -> d -> Bool
  handleAttrs (Node p _) x
#endif
#else
  handleAttrs :: Pattern -> a -> Pattern
--handleAttrs :: Pattern -> a -> Bool
  handleAttrs (Node p _) _
#endif
--- | doTrace as && trace ("HERE! "++show p++"\n"++show as) False = undefined
--- | trace ("HERE! "++show p++"\n"++show as) False = undefined
#if 0
    | uniqueID as ==  4 && trace ("HERE! "++show p++"\n"++show as) False = undefined
    | uniqueID as ==  9 && trace ("HERE! "++show p++"\n"++show as) False = undefined
    | uniqueID as == 11 && trace ("HERE! "++show p++"\n"++show as) False = undefined
#endif
--- | otherwise  = unsafePerformIO $! do
    | otherwise  = unsafePerformIO $ do
--- | otherwise  = trace ("BUHGO!") $ unsafePerformIO $ do
--- | otherwise  = trace ("BUHGO!") $ unsafeDupablePerformIO $ do
       let p0 = p
       p1 <- if doDelay as
             then dly p0 b
             else return p0
#if USE_TRACE_PATNODE
       p2 <- if doTrace as
             then trc p1 b msg_trc
             else return p1
#else
       let p2 = p1
#endif
#if USE_PING_PATNODE
       p3 <- if doPing as
             then png p2 b msg_png
             else return p2
#else
       let p3 = p2
#endif
#if USE_DIE_PATNODE
       p4 <- if doDie as
             then die p3 b msg_die
             else return p3
#else
       let p4 = p3
#endif
#if USE_TIMING_PATNODE
       p5 <- if doTiming as
             then timing p4 b msg_timing
             else return p4
#else
       let p5 = p4
#endif
       return $! Node p5 []
    | otherwise                   = Node p []  -- don't forget!
--- | otherwise                   = p     -- don't forget!
--- | otherwise                   = True  -- don't forget!
   where
#if 1
    b = False  -- WORKED for Ping/png; not working for Trace/trc...
#else
    b = unsafePerformIO $ ( randomIO :: IO Bool )  -- WORKS!!! (even though value is constant!)
#endif
    {-# NOINLINE dly #-}  -- XXX crucial
    dly p b
---  | trace "dly msg!" False = undefined
     | otherwise  = do
         if b
           then do
             !_ <- threadDelay $ delayus as
             return p
--           return $ not b
           else do
             !_ <- threadDelay $ delayus as
             return p
--           return '
#if USE_TRACE_PATNODE
    msg_trc =    "NFDataP: TRACE: " ++ show (uniqueID as)
#if ! HASKELL98_FRAGMENT
              ++ " " ++ show (typeOf x)
#if HANDLE_ATTRS_DATA_CONSTRAINT
              ++ "\n" ++ showRose (shapeOf x)
#endif
#endif
    {-# NOINLINE trc #-}  -- XXX crucial
    trc p b msg
---  | trace "trc msg!" False = undefined
     | otherwise  = do
         if b
           then do
---          !_ <- forkIO $ return (trace msg ())
---          !_ <- forkIO (return (trace msg ()))
             !_ <- trace msg $ return ()
             return p
--           return $ not b
           else do
---          !_ <- forkIO $ return (trace msg ())
---          !_ <- forkIO (return (trace msg ()))
             !_ <- trace msg $ return ()
             return p
--           return '
#endif
#if USE_PING_PATNODE
    msg_png =    "NFDataP: PING: " ++ show (uniqueID as)
#if ! HASKELL98_FRAGMENT
              ++ " " ++ show (typeOf x)
#if HANDLE_ATTRS_DATA_CONSTRAINT
              ++ "\n" ++ showRose (shapeOf x)
#endif
#endif
    {-# NOINLINE png #-}  -- XXX crucial
    -- Consider mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
    png p b msg
---  | trace "png msg!" False = undefined
#if 1
     | isNothing mpngtid  = do
--        b <- randomIO :: IO Bool
---      let b = False in
         if b
           then do
             return p
--           return $ not b
           else do
             return p
--           return b
#else
     | isNothing mpngtid  = False
#endif
     | otherwise  = do
--        b <- randomIO :: IO Bool
---      let b = False in
         if b
           then do
             !_ <- forkIO $ throw $ DeepSeqBounded_PingException msg
---          putStrLn "Carrying on FALSE ..."
             return p
--           return $ not b
           else do
             -- This worked! (exception thrown, yet continues)
             -- Getting repeatable actions still eludes...
--           evaluate (unsafeInterleaveIO (do
--           evaluate (unsafeDupablePerformIO (do
             !_ <- forkIO $ throw $ DeepSeqBounded_PingException msg
---          putStrLn "Carrying on TRUE ..."
             return p
--           return b
     where mpngtid = pingParentTID as
#endif
#if USE_DIE_PATNODE
    msg_die =    "NFDataP: DIE: " ++ show (uniqueID as)
#if ! HASKELL98_FRAGMENT
              ++ " " ++ show (typeOf x)
#if HANDLE_ATTRS_DATA_CONSTRAINT
              ++ "\n" ++ showRose (shapeOf x)
#endif
#endif
    {-# NOINLINE die #-}  -- XXX crucial (except perhaps in this die case...)
    die p b msg = do
      if b
        then do
          putStrLn msg >> myThreadId >>= killThread
          return p
--        return $ not b
        else do
          putStrLn msg >> myThreadId >>= killThread
          return p
--        return b
#endif
#if USE_TIMING_PATNODE
    msg_timing =    "NFDataP: TIMING: " ++ show (uniqueID as)
#if ! HASKELL98_FRAGMENT
                 ++ " " ++ show (typeOf x)
#if HANDLE_ATTRS_DATA_CONSTRAINT
                 ++ "\n" ++ showRose (shapeOf x)
#endif
#endif
    {-# NOINLINE timing #-}
    timing p b msg = do
      if b
        then do
          -- ... XXX
          return p
--        return $ not b
        else do
          -- ... XXX
          return p
--        return b
#endif
    as = getPatNodeAttrs p  -- XXX THIS IS BAD (bottleneck)
             -- [See 000-readme, cotemp 20150104.]
             -- Is there not a simpler way to get at the PatNodeAttr?
             -- One thing could do, is put the node type (WR, etc.)
             -- in PatNodeAttrs, and then PatNode = PatNodeAttrs.
             -- There's no real disadvantage, right? -- we can still
             -- pattern match almost as do now:
             --   (Node WR{} cs)               -- now
             --   (Node PN{nodeKind=WR}  cs)   -- hopefully!
#endif

-------------------------------------------------------------------------------

#if USE_PSEQ_PATNODE

  -- Note that, if USE_PSEQ_PATNODE flag is True, then Control.Parallel.pseq
  -- is used instead of Prelude.seq, whether or not a >cdba permutation was
  -- specified.  I'd kinda rather continue to use seq for the cases where
  -- no permutation was specified...
  pseq_condition :: Pattern -> [()] -> ()
--pseq_condition :: [Pattern] -> [()] -> ()
--pseq_condition :: [Pattern] -> [ Pattern -> x -> () ] -> ()
--pseq_condition :: [Pattern] -> [Pattern -> x -> ()] -> [Pattern -> x -> ()]
#if 0
#elif 1
  pseq_condition pat@(Node pn cs) fs
   | isNothing mperm  = foldr  seq () fs
   | otherwise        = foldr pseq () $ map (\i->(fs!!i)) perm
   where
    mperm = pseqPerm $ getPatNodeAttrs pn
    perm = fromJust mperm
#elif 0
  pseq_condition pat@(Node pn cs) fs
   = foldr pseq () fs'  -- is foldr the right fold?
   where
    mperm = pseqPerm $ getPatNodeAttrs pn
    perm = fromJust mperm
    fs' | isNothing mperm  = fs
        | otherwise        = map (\i->(fs!!i)) perm
#elif 0
  pseq_condition pats fs = foldr seq () fs  -- is foldr the right fold?
#endif

#endif

-------------------------------------------------------------------------------

