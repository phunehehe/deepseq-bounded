
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#define USE_TRACE 1

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

  {-  LANGUAGE AllowAmbiguousTypes #-}

  {-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Generic.GNFDataN
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Support for generic deriving (via <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>) of 'NFDataN' instances.
--
-- 'NFDataN' does not have any superclasses.
--
-- It is also possible to derive instances using "GHC.Generics", which
-- avoids SOP and TH, but if you plan to use 'NFDataP' then SOP is required.
-- (SOP can be used without TH if necessary; the interested reader is
-- referred to SOP documentation.)
--
-- This metaboilerplate is standard for using the generic deriving
-- facilities of "GHC.Generics" and <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>.
-- Consider <http://hackage.haskell.org/package/seqaid seqaid> for a
-- turnkey solution.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE GADTs #-}  -- for GHC < 7.8 (== 7.6.3)
-- > 
-- > import Generics.SOP.TH
-- > import Control.DeepSeq.Bounded ( NFDataN(..), grnfn )
-- > import GHC.Generics ( Generic )
-- >
-- > import Control.DeepSeq.Bounded ( forcen )
-- > 
-- > data TA = A1 TB TA | A2  deriving ( Generic )
-- > instance NFDataN TA where rnfn = grnfn
-- > 
-- > data TB = B1 Int | B2 TA  deriving ( Generic )
-- > instance NFDataN TB where rnfn = grnfn
-- > 
-- > deriveGeneric ''TA
-- > deriveGeneric ''TB
-- >
-- > main = return $! forcen 3 (A1 (B2 undefined) A2)

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Generic.GNFDataN
  (

#if USE_SOP
      grnfn
#else
      genericRnfn
--  , genericRnfnV1
#endif

#if 0
      -- * "Control.DeepSeq" re-exports
    , deepseq
    , force
    , NFData(rnf)
    , ($!!)
#endif

  )
  where

-------------------------------------------------------------------------------

#if ! JUST_ALIAS_GNFDATAN
  import Control.DeepSeq.Bounded.NFDataN
#endif

#if USE_SOP
  import Generics.SOP
--import Generics.SOP.TH  -- not here, but rather in the module needing to generically derive an NFDataN instance
#else
  import GHC.Generics
#endif

#if 0
  -- actually can be used in the SOP implementation (and is in the
  -- example in the paper as well as the API docs), not that we nec.
  -- want this; but there's no n=infinity rnfn, so I don't see how
  -- else to manage it... after the "collapse"...
  import Control.DeepSeq ( NFData, deepseq )
--import Control.DeepSeq ( rnf )  -- actually can be used in the SOP implementation, not that we necessarily want this; but there's no n=infinity rnfn, so I don't see how else to manage it... after the "collapse"...
#if 0
  import Control.DeepSeq  -- needed?
#endif
#endif

  import Debug.Trace ( trace )

-------------------------------------------------------------------------------

#if USE_SOP

#if JUST_ALIAS_GNFDATAN

  grnfn :: (Generic a, All2 Generic (Code a)) => Int -> a -> ()
  grnfn n x = grnfnS n (from x)

  grnfnS :: forall yss y. (All2 Generic yss, All2 Generic (Code y)) => Int -> SOP I yss -> ()
--grnfnS :: Int -> SOP I xss -> ()
  grnfnS n (SOP (S xss)) = grnfnS n (SOP xss)
  grnfnS n (SOP (Z xs))  = grnfnP (-1+n) xs
   where
    grnfnP :: forall ys. (All Generic ys) => Int -> NP I ys -> ()
--  grnfnP :: forall ys. (All Generic ys, All2 Generic (Code y)) => Int -> NP I ys -> ()
--  grnfnP :: (All Generic xs) => Int -> NP I xs -> ()
--  grnfnP :: Int -> NP I xs -> ()
    grnfnP n Nil          = ()
    grnfnP n (I x :* xs)
     | n <= 0             = ()
     | otherwise          = rnfn n x `seq` grnfnP n xs

  rnfn :: (Generic a, All2 Generic (Code a)) => Int -> a -> ()
--rnfn :: Generic a => Int -> a -> ()
  rnfn n x = grnfn n x

#else

#if 1

  grnfn :: (Generic a, All2 NFDataN (Code a)) => Int -> a -> ()
  grnfn n x = grnfnS n (from x)

  grnfnS :: (All2 NFDataN xss) => Int -> SOP I xss -> ()
  grnfnS n (SOP (Z xs))  = grnfnP (-1+n) xs
  grnfnS n (SOP (S xss)) = grnfnS n (SOP xss)

  grnfnP :: (All NFDataN xs) => Int -> NP I xs -> ()
  grnfnP n Nil          = ()
  grnfnP n (I x :* xs)
   | n <= 0             = ()
   | otherwise          = rnfn n x `seq` grnfnP n xs

#else

  -- XXX NOPE! This causes decrementing as traverse ctor args!
  -- However, the above explicit recursive version works!
  grnfn :: (Generic a, All2 NFDataN (Code a)) => Int -> a -> ()
-- Ah-hah!
  grnfn n = rnfn n . hcollapse . hcliftA p (K . (if n <= 0 then const () else rnfn (-1+n)) . unI) . from
-- This doesn't help:
--grnfn n = let n_ = (-1+n) in
--          rnfn n . hcollapse . hcliftA p (K . rnfn n_ . unI) . from
-- So this is the closest I have so far, but it's broken b/c it
-- seems to add to the requisite depth, the index of the (leftmost?)
-- sibling bearing "undefined".
--grnfn n = rnfn n . hcollapse . hcliftA p (K . rnfn (-1+n) . unI) . from
-- This just delays everything by one more.
--grnfn n = rnfn (-1+n) . hcollapse . hcliftA p (K . rnfn (-1+n) . unI) . from
-- I don't think this was my problem anyhow, as arities aren't
-- high enough to expect it to affect depth of n [??...]
-- (And it doesn't work anyhow -- way too much stuff gets forced!)
--grnfn n = rnf    . hcollapse . hcliftA p (K . rnfn (-1+n) . unI) . from
    where p = Proxy :: Proxy NFDataN
-- From the SOP paper:
--   grnfn :: (Generic a, All2 NFDataN (Code a)) => a -> ()
--   grnfn = rnfn . hcollapse . hcliftA p (K . rnf . unI) . from
--     where p = Proxy :: Proxy NFDataN
-- "We can understand this function by tracking the types. First
-- we use from to translate from a to the generic representation
-- SOP I (Code a). We then map rnf (modulo newtype wrapping and
-- unwrapping) across this sum of products to get a value of type
-- SOP (K ()) (Code a), which we can collapse to a list of type [()].
-- Finally, we can reduce that list to a single unit value through one
-- more application of rnf.  We use All2 in the type of grnf to require
-- that the types of the leaves must all satisfy NFData."

#endif

#endif

-------------------------------------------------------------------------------

#else

  genericRnfn :: (Generic a, GNFDataN (Rep a)) => Int -> a -> ()
  genericRnfn n = grnfn_ n . from
--genericRnfn n = grnfn_ (-1+n) . from
  {-# INLINE genericRnfn #-}

--  Hidden internal type-class
--
-- Note: the 'V1' instance is not provided for 'GNFDataN' in order to
-- trigger a compile-time error; see 'GNFDataNV1' which defers this to
-- a runtime error.
  class GNFDataN f where
    grnfn_ :: Int -> f a -> ()
#if 1 || USE_TRACE
    grnfn_ n x = trace "HH-0" $ ()  -- never seen, so far...
#else
    grnfn_ n x = ()
#endif
--  grnfn_ n x = rnfn n $ to x
--  grnfn_ n x = rnfn n x

  instance GNFDataN U1 where
#if USE_TRACE
    grnfn_ _ !U1 = trace "HH-U1" $ ()
#else
    grnfn_ _ !U1 = ()
#endif
    {-# INLINE grnfn_ #-}

  instance NFDataN a => GNFDataN (K1 i a) where
#if USE_TRACE
#if 1
    grnfn_ n k@(K1 x) | n <= 0     = trace "()-K1" $ ()
--                    | otherwise  = trace "HH-K1" $ k `seq` rnfn (-1+n) x
--                    | otherwise  = trace "HH-K1" $ x `seq` rnfn (-1+n) x
                      | otherwise  = trace "HH-K1" $ rnfn (-1+n) x
#else
    grnfn_ n | n <= 0     = const ()
             | otherwise  = trace "HH-2" $ rnfn (-1+n) . unK1
#endif
#else
    grnfn_ n (K1 x) | n <= 0     = ()
                    | otherwise  = rnfn (-1+n) x
#endif
    {-# INLINE grnfn_ #-}

  instance GNFDataN a => GNFDataN (M1 i c a) where
#if USE_TRACE
#if 0
#elif 1
    grnfn_ n (M1 x) | n <= 0     = trace "()-M1" $ ()  -- prob. unnec.
                    | otherwise  = trace "HH-M1" $ grnfn_ n x
#elif 0
    grnfn_ n (M1 x) | n <= 0     = trace "()-M1" $ ()
                    | otherwise  = trace "HH-M1" $ grnfn_ (-1+n) x
#elif 0
    grnfn_ n | n <= 0     = const ()
             | otherwise  = trace "HH-3" $ grnfn_ (-1+n) . unM1
#endif
#else
    grnfn_ n (M1 x) | n <= 0     = ()  -- prob. unnec.
                    | otherwise  = grnfn_ n x
#endif
    {-# INLINE grnfn_ #-}

  instance (GNFDataN a, GNFDataN b) => GNFDataN (a :*: b) where
#if USE_TRACE
#if 0
#elif 1
    grnfn_ n (x :*: y) | n <= 0     = trace "()-:*:" $ ()  -- prob. unnec.
                       | otherwise  = trace "HH-:*:" $ let n' = n in grnfn_ n' x `seq` grnfn_ n' y
#elif 0
    grnfn_ n (x :*: y) | n <= 0     = trace "()-:*:" $ ()
                       | otherwise  = trace "HH-:*:" $ let n' = -1+n in grnfn_ n' x `seq` grnfn_ n' y
#elif 0
    grnfn_ n (x :*: y) | n <= 0     = ()
                       | otherwise  = trace "HH-4" $ let n' = -1+n in grnfn_ n' x `seq` grnfn_ n' y
#endif
#else
    grnfn_ n (x :*: y) | n <= 0     = ()  -- prob. unnec.
                       | otherwise  = let n' = n in grnfn_ n' x `seq` grnfn_ n' y
#endif
    {-# INLINE grnfn_ #-}

  instance (GNFDataN a, GNFDataN b) => GNFDataN (a :+: b) where
#if USE_TRACE
#if 0
#elif 1
    grnfn_ n (L1 x) | n <= 0     = trace "()-L1" $ ()  -- prob. unnec.
                    | otherwise  = trace "HH-L1" $ grnfn_ n x
    grnfn_ n (R1 x) | n <= 0     = trace "()-L2" $ ()  -- prob. unnec.
                    | otherwise  = trace "HH-L2" $ grnfn_ n x
#elif 0
    grnfn_ n (L1 x) | n <= 0     = trace "()-L1" $ ()
                    | otherwise  = trace "HH-L1" $ grnfn_ (-1+n) x
    grnfn_ n (R1 x) | n <= 0     = trace "()-L2" $ ()
                    | otherwise  = trace "HH-L2" $ grnfn_ (-1+n) x
#elif 0
    grnfn_ n (L1 x) | n <= 0     = ()
                    | otherwise  = trace "HH-5L" $ grnfn_ (-1+n) x
    grnfn_ n (R1 x) | n <= 0     = ()
                    | otherwise  = trace "HH-5R" $ grnfn_ (-1+n) x
#endif
#else
    grnfn_ n (L1 x) | n <= 0     = ()  -- prob. unnec.
                    | otherwise  = grnfn_ n x
    grnfn_ n (R1 x) | n <= 0     = ()  -- prob. unnec.
                    | otherwise  = grnfn_ n x
#endif
    {-# INLINE grnfn_ #-}

#endif

-------------------------------------------------------------------------------

