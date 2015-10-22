
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#define USE_TRACE 1

#define SEQHARN_WRAP_PARENT 1

-------------------------------------------------------------------------------

#if USE_SOP
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ < 708
  {-# LANGUAGE GADTs #-}
#endif
  {-# LANGUAGE TemplateHaskell #-}
#else
  {-# LANGUAGE BangPatterns #-}
  {-# LANGUAGE TypeOperators #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-  LANGUAGE MultiParamTypeClasses #-}
  {-  LANGUAGE Rank2Types #-}
#endif

  {-# LANGUAGE ScopedTypeVariables #-}
  {-  LANGUAGE AllowAmbiguousTypes #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Generic.GSeqable
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Generic function version of <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-Seqable.html Seqable> (via <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>).
--
-- Probably, a "GHC.Generics" variant would also be possible.
--
-- This metaboilerplate is standard for using the generic deriving
-- facilities of <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>.
-- Consider <http://hackage.haskell.org/package/seqaid seqaid>
-- for a turnkey solution.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE GADTs #-}  -- for GHC < 7.8 (== 7.6.3)
-- > 
-- > import Generics.SOP.TH
-- > import Control.DeepSeq.Bounded.Seqable
-- > 
-- > data TA = A1 TB TA | A2
-- > data TB = B1 Int | B2 TA
-- > 
-- > deriveGeneric ''TA
-- > deriveGeneric ''TB
-- > 
-- > main = return $! force_ Propagate (A1 (force_ Propagate (B2 undefined)) A2)

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Generic.GSeqable
  (

#if USE_SOP
      grnf_
    , gseq_
    , gforce_
#else
      genericSeq_
--  , genericSeq_V1
#endif

    , seqharn

  )
  where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded.Pattern ( SeqNode(..) )
--import Control.DeepSeq.Bounded.Seqable ( SeqNode(..) )

#if USE_SOP
  import Generics.SOP
--import Generics.SOP.TH  -- not here, but rather in the module needing to generically derive an Seqable instance
#else
  import GHC.Generics
#endif

  import Control.Parallel ( par )

  import Data.Typeable ( typeOf )
  import Data.Typeable ( Typeable )

  import Debug.Trace ( trace )

-------------------------------------------------------------------------------

#if USE_SOP

  gseq_ :: Generic a => SeqNode -> a -> b -> b
  gseq_ Insulate     a b  =                 b
  gseq_ k            a b  = grnf_ k a `seq` b  -- sic! both Propagate and Spark

  gforce_ :: Generic a => SeqNode -> a -> a
  gforce_ Insulate     a  =                 a
  gforce_ k            a  = grnf_ k a `seq` a  -- sic! both Propagate and Spark

  grnf_ :: Generic a => SeqNode -> a -> ()
  grnf_ Insulate x = ()
  grnf_ k x = grnf_S k (from x)

  grnf_S :: SeqNode -> SOP I xss -> ()
#if USE_PAR_SEQABLE
  grnf_S Propagate       (SOP (Z xs))   = grnf_P xs `seq` ()
  grnf_S {-Spark-}_      (SOP (Z xs))   = grnf_P xs `par` ()
#else
  grnf_S {-Propagate-}_  (SOP (Z xs))   = grnf_P xs `seq` ()
#endif
  grnf_S k               (SOP (S xss))  = grnf_S k (SOP xss)

  grnf_P :: NP I xs -> ()
  grnf_P Nil = ()
  grnf_P (I x :* xs) = x `seq` grnf_P xs

-------------------------------------------------------------------------------

#if 1

  -- | @'seqharn' x@ is semantically the same as @x@, except its
  -- strictness, parallellism, etc. can be tweaked dynamically...
  --
  -- > seqharn = to . hliftA (gforce_ Insulate) . from
  --
  -- /I can see how this would be useful at compile-time, but how can we use this if seqharn only runs post-compilation?  Or is it just analogous to forcep?.../
  --
  -- /Also: How exactly to/ dynamically configure /this?.../
  
  seqharn :: Generic a => a -> a
-- hliftA f xs = hpure (fn f) ` hap ` xs
  seqharn = to . hliftA (gforce_ Insulate) . from
--seqharn x = to $ hliftA (gforce_ Insulate) $ from x
--seqharn x = to $ hpure (fn $ gforce_ Insulate) `hap` from x
--seqharn x = to (hap (hpure (fn (gforce_ Insulate))) (from x))
--seqharn x = to $ hap (hpure (fn $ gforce_ Insulate)) $ from x

#if 0
  -- | The 'PatNode's of the 'Pattern' argument carry 'SeqNode'
  -- programmability information in the 'seqNode' field.
  -- The 'PatNode' must be 'WR' for interior nodes and 'WS' for leaves,
  -- such as returned by 'mkPat'.
  -- Pattern match is an error.
  configure_seqharn :: Generic a => Pattern -> SeqNode a -> SeqNode a
  configure_seqharn (Node pn cs) (SeqNode k x)
   | WI{} <- pn  = ...
#endif

-- No joy.
-- What if just replace every node with a SeqNode'?
-- But that's what I'm trying to do...

#if 0

  data SeqNode' a =
           Insulate' a
---      | Conduct' a
         | Propagate' a
#if USE_PAR_SEQABLE
         | Spark' a
#endif
    deriving ( Eq, Ord )
  unSeqNode' :: SeqNode' a -> a
  unSeqNode' (Insulate' x) = x
  unSeqNode' (Propagate' x) = x
#if USE_PAR_SEQABLE
  unSeqNode' (Spark' x) = x
#endif

#if 1

-- Yet here it is in SYB: ... oops, no, only if
-- the user passes the generic function in (familiar
-- quandry, esp from sai-shape-syb)...
  testboo :: Data a => GenericT -> a -> SeqNode' a
  testboo fg x = everywhere fg x
   where
    fg :: ...

#else

-- XXX Maybe this just isn't possible (and fortunately the function
-- approach even works!...) -- it seems like I want to say here
-- that SeqNode' a is equivalent to a as a type, but that's not
-- correct -- they will be equivalent, in some sense, as VALUES
-- (in a given term context), but they are not equivalent as types.
-- In particular, SeqNode' a has arity 1 for every constructor, but
-- a itself might be of any arity.
--   Still maybe there's some way ... the AST makes sense, anyway...
-- If we have a function a -> a, we can lift it to SeqNode' a -> SeqNode' a.
-- Or we can us unSeqNode'?...
--------
-- Note that TypeRep (returned by typeOf) does have an Eq instance.
-- However, I think the problem of supplying lists of TypeRep
-- instead of String type names, for stoptys argument, was
-- a deterrent...
--testboo :: forall a. (SeqNode' a ~ a, Generic a) =>
  testboo :: Generic a => [String] -> a -> SeqNode' a
--testboo :: (Typeable a, Generic a) => [String] -> a -> SeqNode' a
--testboo :: (Typeable a, Generic a, All2 Typeable (Code a)) => [String] -> a -> SeqNode' a
  testboo stoptys x = x''
   where
--  proxy = Proxy :: a
--  tstr = show $ typeOf x
--  x' = to $ hpure (fn (testboo stoptys)) `hap` from x
--  x' = to $ hcliftA proxy (unSeqNode' . testboo stoptys) $ from x
    x' = to $ hliftA (testboo stoptys) $ from x
--- x' = to $ hpure (I . fn . testboo stoptys . unI) `hap` from x
--- x' = to $ hpure (fn $ I . testboo stoptys . unI) `hap` from x
--  x' = to $ hpure (fn $ testboo stoptys) `hap` from x
#if 0
    x'' | elem tstr stoptys  = Insulate' x'
        | otherwise          = Propagate' x'
#else
    x'' | True       = Insulate' x
        | otherwise  = Propagate' x
#endif

#endif

#endif

#else

#if SEQHARN_WRAP_PARENT

  -- | @'seqharn' x@ is semantically the same as @x@, except its
  -- strictness, parallellism, etc. can be tweaked dynamically...
  seqharn :: Generic a => a -> a
  seqharn x = gforce_ Insulate (seqharnS (from x))

  -- From the sum, select the alternative corresponding to the term node.
  -- This alternative is one of the constructors of the data type,
  -- with its arguments represented as a product.
  seqharnS :: SOP I xss -> a
  seqharnS (SOP (Z xs))   = seqharnP xs
  seqharnS (SOP (S xss))  = seqharnS (SOP xss)

  -- Traverse the constructor arguments of the product.
  seqharnP :: NP I xs -> a
  seqharnP Nil = ()
  seqharnP (I x :* xs) = seqharn x `seq` seqharnP xs

#else

  -- | @'seqharn' x@ is semantically the same as @x@, except its
  -- strictness, parallellism, etc. can be tweaked dynamically...
  seqharn :: Generic a => a -> a
  seqharn x = seqharnS (from x)

  -- From the sum, select the alternative corresponding to the term node.
  -- This alternative is one of the constructors of the data type,
  -- with its arguments represented as a product.
  seqharnS :: SOP I xss -> a
  seqharnS (SOP (Z xs))   = seqharnP xs
  seqharnS (SOP (S xss))  = seqharnS (SOP xss)

  -- Traverse the constructor arguments of the product.
  seqharnP :: NP I xs -> a
  seqharnP Nil = ()
  seqharnP (I x :* xs) = (gforce_ Insulate (seqharn x)) `seq` seqharnP xs

#endif

#endif

-------------------------------------------------------------------------------

#else

#error "GSeqable: must use SOP for now..."

#endif

-------------------------------------------------------------------------------

