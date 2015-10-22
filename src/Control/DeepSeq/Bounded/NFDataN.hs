
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}  -- specified in .cabal default-extensions

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

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.NFDataN
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides an overloaded function, 'deepseqn', for partially
-- (or fully) evaluating data structures to a given depth.
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.NFDataN
  (

      -- * Depth-bounded analogues of 'deepseq' and 'force'

      deepseqn
    , forcen

      -- * Class of things that can be evaluated to an arbitrary finite depth

#if JUST_ALIAS_GNFDATAN
    , rnfn
#else
    , NFDataN(..)
#endif

  )
  where

-------------------------------------------------------------------------------

--import qualified Data.Generics.Shape as S  -- not actually used

#if JUST_ALIAS_GNFDATAN
  import Generics.SOP ( Generic )
  import Control.DeepSeq.Bounded.Generic.GNFDataN ( grnfn )
#endif

#if USE_NFDATA_SUPERCLASS
  import Control.DeepSeq
#endif

--import Data.Data

  import Data.Int
  import Data.Word
  import Data.Ratio
  import Data.Complex
  import Data.Array
  import Data.Fixed
  import Data.Version

-------------------------------------------------------------------------------

#if JUST_ALIAS_GNFDATAN
  type NFDataN = Generic
#endif

#if JUST_ALIAS_GNFDATAN
  rnfn = grnfn
#endif

-------------------------------------------------------------------------------

-- infixr 0 $!!

-------------------------------------------------------------------------------

  -- | @'deepseqn' n x y@ evaluates @x@ to depth n, before returning @y@.
  --
  -- This is used when expression @x@ also appears in @y@, but mere
  -- evaluation of @y@ does not force the embedded @x@ subexpression
  -- as deeply as we wish.
  --
  -- The name 'deepseqn' is used to illustrate the relationship to 'seq':
  -- where 'seq' is shallow in the sense that it only evaluates the top
  -- level of its argument, @'deepseqn' n@ traverses (evaluates) the entire
  -- top @n@ levels of the data structure.
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
  -- and as a less indiscriminate substitute for 'deepseq'.
  --
  -- Furthermore, 'deepseqn' can sometimes be a better solution than
  -- using stict fields in your data structures, because the
  -- latter will behave strictly everywhere that its constructors
  -- are used, instead of just where its laziness is problematic.
  --
  -- There may be possible applications to the prevention of resource leaks
  -- in lazy streaming, but I'm not certain.
  --
  -- One of the special qualities of 'NFDataN' is shape oblivity:
  -- it doesn't care about any details of the shape of the term
  -- it's forcing, it only cares about stratifying levels of
  -- recursion depth.
  -- (I would say \"as contrasted with <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html NFDataP>\" but cannot, because
  -- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html NFDataP>
  -- was extended to include
  -- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataN.html NFDataN>
  -- syntax\/capabilities, precisely to ammend this deficiency.)

  deepseqn :: NFDataN a => Int -> a -> b -> b
  deepseqn n a b = rnfn n a `seq` b
#if 1
  -- XXX Need to double-check that this makes sense; didn't think
  -- it through -- when b != a, things are not so simple.
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-# RULES
    "deepseqn/composition"    forall n1 n2 x.  (.) (deepseqn n2) (deepseqn n1) x = deepseqn ( max n1 n2 ) x
      #-}
#endif

-------------------------------------------------------------------------------

#if 0
-- As per DeepSeq:
  -- | the deep analogue of '$!'.  In the expression @f $!! x@, @x@ is
  -- fully evaluated before the function @f@ is applied to it.
  ($!!) :: NFData a => (a -> b) -> a -> b
  f $!! x = x `deepseq` f x
#endif

-------------------------------------------------------------------------------

  -- | 'forcen' is a variant of 'deepseqn' that is useful in some circumstances.
  --
  -- > forcen n x = deepseqn n x x
  --
  -- @forcen n x@ evaluates @x@ to depth @n@, and then returns it.
  -- Recall that, in common with all Haskell functions, @forcen@ only
  -- performs evaluation when upstream demand actually occurs,
  -- so essentially it turns shallow evaluation into depth-@n@ evaluation.

  forcen :: NFDataN a => Int -> a -> a
  forcen n x = deepseqn n x x
#if 1
  {-# RULES
    "forcen/composition"    forall n1 n2 x.  (.) (forcen n2) (forcen n1) x = forcen ( max n1 n2 ) x
      #-}
#endif

-------------------------------------------------------------------------------

#if ! JUST_ALIAS_GNFDATAN

  -- | A class of types that can be evaluated to arbitrary depth.
#if USE_NFDATA_SUPERCLASS
  class NFData a => NFDataN a where
#else
  class NFDataN a where
#endif
#if 0
    -- | rnf should reduce its argument to normal form (that is, fully
    -- evaluate all sub-components), and then return '()'.
    --
    -- The default implementation of 'rnf' is
    --
    -- > rnf a = a `seq` ()
    --
    -- which may be convenient when defining instances for data types with
    -- no unevaluated fields (e.g. enumerations).
    rnf :: a -> ()
    rnf a = a `seq` ()
#endif
    rnfn :: Int -> a -> ()
#if USE_NFDATA_SUPERCLASS
    -- ?? not sure about this (whatever, we'll override it)
#if 1
    rnfn n a | n <= 0     = ()  -- case needed? (seems to be!)
             | otherwise  = rnf a
#else
    rnfn _ a = rnf a
#endif
#else
#if 0
#elif 1
    rnfn n x | n <= 0     = ()
             | otherwise  = x `seq` ()
#elif 0
    rnfn _ x = x `seq` ()
#elif 0
    rnfn _ _ = ()
#endif
#endif
#if 1
  {-# RULES
    "rnfn/composition"    forall n1 n2 x.  (.) (rnfn n2) (rnfn n1) x = rnfn ( max n1 n2 ) x
      #-}
--   "rnfp/composition"    forall p1 p2 x.  compose (rnfp p2) (rnfp p1) x = rnfp ( unionPat [p1, p2] ) x
--   "rnfp/composition"    forall p1 p2 x.  ( rnfp p2 . rnfp p1 ) x = rnfp ( unionPat [p1, p2] ) x
#endif

-------------------------------------------------------------------------------

  instance NFDataN Int
  instance NFDataN Word
  instance NFDataN Integer
  instance NFDataN Float
  instance NFDataN Double

  instance NFDataN Char
  instance NFDataN Bool
  instance NFDataN ()

  instance NFDataN Int8
  instance NFDataN Int16
  instance NFDataN Int32
  instance NFDataN Int64

  instance NFDataN Word8
  instance NFDataN Word16
  instance NFDataN Word32
  instance NFDataN Word64

  instance NFDataN (Fixed a)

  -- [Quoted from deepseq:]
  -- This instance is for convenience and consistency with 'seq'.
  -- This assumes that WHNF is equivalent to NF for functions.
  instance NFDataN (a -> b)

#if 0
  instance NFDataN a => NFDataN (IO a) where
    rnfn !n x = ()  -- XXX ignore if in IO (cludge easing an experiment...)
#endif

  -- not taken to be a level of depth
  instance (Integral a, NFDataN a) => NFDataN (Ratio a) where
    rnfn !n x = rnfn n (numerator x, denominator x)

  instance (RealFloat a, NFDataN a) => NFDataN (Complex a) where
#if 1
    rnfn n (x:+y) = case n of
     n | n <= 0  -> ()
     _           -> let n' = -1+n in
                    rnfn n' x `seq`
                    rnfn n' y `seq`
                    ()
#else
    rnfn !n (x:+y)
     | n <= 0    = ()
     | otherwise = let n' = -1+n in
                   rnfn n' x `seq`
                   rnfn n' y `seq`
                   ()
#endif

  instance NFDataN a => NFDataN (Maybe a) where
    rnfn _ Nothing  = ()
    rnfn !n (Just x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x

  instance (NFDataN a, NFDataN b) => NFDataN (Either a b) where
    rnfn !n (Left x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
    rnfn !n (Right y)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) y

  instance NFDataN Data.Version.Version where
    rnfn !n (Data.Version.Version branch tags)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' branch `seq` rnfn n' tags

  instance NFDataN a => NFDataN [a] where
    rnfn _ [] = ()
    rnfn !n (x:xs)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x `seq` rnfn n' xs

  -- not taken to be a level of depth
  instance (Ix a, NFDataN a, NFDataN b) => NFDataN (Array a b) where
    rnfn !n x = rnfn n (bounds x, Data.Array.elems x)

  instance (NFDataN a, NFDataN b) => NFDataN (a,b) where
    rnfn !n (x,y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x `seq` rnfn n' y

  instance (NFDataN a, NFDataN b, NFDataN c) => NFDataN (a,b,c) where
    rnfn !n (x,y,z)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x `seq` rnfn n' y `seq` rnfn n' z

  instance (NFDataN a, NFDataN b, NFDataN c, NFDataN d) => NFDataN (a,b,c,d) where
    rnfn !n (x1,x2,x3,x4)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4

  instance (NFDataN a1, NFDataN a2, NFDataN a3, NFDataN a4, NFDataN a5) =>
           NFDataN (a1, a2, a3, a4, a5) where
    rnfn !n (x1,x2,x3,x4,x5)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4 `seq`
                    rnfn n' x5

  instance (NFDataN a1, NFDataN a2, NFDataN a3, NFDataN a4, NFDataN a5, NFDataN a6) =>
           NFDataN (a1, a2, a3, a4, a5, a6) where
    rnfn !n (x1,x2,x3,x4,x5,x6)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4 `seq`
                    rnfn n' x5 `seq`
                    rnfn n' x6

  instance (NFDataN a1, NFDataN a2, NFDataN a3, NFDataN a4, NFDataN a5, NFDataN a6, NFDataN a7) =>
           NFDataN (a1, a2, a3, a4, a5, a6, a7) where
    rnfn !n (x1,x2,x3,x4,x5,x6,x7)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4 `seq`
                    rnfn n' x5 `seq`
                    rnfn n' x6 `seq`
                    rnfn n' x7

  instance (NFDataN a1, NFDataN a2, NFDataN a3, NFDataN a4, NFDataN a5, NFDataN a6, NFDataN a7, NFDataN a8) =>
           NFDataN (a1, a2, a3, a4, a5, a6, a7, a8) where
    rnfn !n (x1,x2,x3,x4,x5,x6,x7,x8)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4 `seq`
                    rnfn n' x5 `seq`
                    rnfn n' x6 `seq`
                    rnfn n' x7 `seq`
                    rnfn n' x8

  instance (NFDataN a1, NFDataN a2, NFDataN a3, NFDataN a4, NFDataN a5, NFDataN a6, NFDataN a7, NFDataN a8, NFDataN a9) =>
           NFDataN (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    rnfn !n (x1,x2,x3,x4,x5,x6,x7,x8,x9)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in
                    rnfn n' x1 `seq`
                    rnfn n' x2 `seq`
                    rnfn n' x3 `seq`
                    rnfn n' x4 `seq`
                    rnfn n' x5 `seq`
                    rnfn n' x6 `seq`
                    rnfn n' x7 `seq`
                    rnfn n' x8 `seq`
                    rnfn n' x9

#endif

-------------------------------------------------------------------------------

