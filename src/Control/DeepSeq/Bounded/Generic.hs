
-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Generic
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Support for generic deriving (via <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>) of
-- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataN.html#t:NFDataN NFDataN> and
-- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP NFDataP> instances.
-- Also, @SOP@ generic functions implementing <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-Seqable.html Seqable> without a class and instances.
--
-- This metaboilerplate is standard for using the generic deriving facilities of <http://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/GHC-Generics.html GHC.Generics> and <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>.  Consider <http://hackage.haskell.org/package/seqaid seqaid> for a turnkey solution.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE GADTs #-}  -- for GHC < 7.8 (== 7.6.3)
-- > 
-- > import Generics.SOP.TH
-- > import Control.DeepSeq.Bounded ( NFDataN(..), grnfn, NFDataP(..), grnfp )
-- > import Control.DeepSeq.Generic ( NFData(..), genericRnf )
-- > import GHC.Generics ( Generic )    -- for deriving NFData
-- > import Data.Typeable ( Typeable )  -- for name-constrained pattern nodes
-- > import Control.DeepSeq.Bounded ( forcen, forcep )
-- > 
-- > data TA = A1 TB TA | A2  deriving ( Generic, Typeable )
-- > instance NFData  TA where rnf  = genericRnf
-- > instance NFDataN TA where rnfn = grnfn
-- > instance NFDataP TA where rnfp = grnfp
-- > 
-- > data TB = B1 Int | B2 TA  deriving ( Generic, Typeable )
-- > instance NFData  TB where rnf  = genericRnf
-- > instance NFDataN TB where rnfn = grnfn
-- > instance NFDataP TB where rnfp = grnfp
-- > 
-- > deriveGeneric ''TA
-- > deriveGeneric ''TB
-- > 
-- > main = mainP
-- > mainN = return $! forcen 3         (A1 (B2 undefined) A2) :: IO TA
-- > mainP = return $! forcep "((!).)"  (A1 (B2 undefined) A2) :: IO TA
-- > mainS = return $! force_ Propagate (A1 (force_ Propagate (B2 undefined)) A2) :: IO TA

-------------------------------------------------------------------------------

-- GHC 7.10.1-rc1 : for some reason with this compiler, Cabal 1.22
-- (the earliest Cabal that works with GHC 7.10) does not properly
-- initialise the USE_SOP flag to True, despite .cabal file default.
-- Some flags seem fine, but not this one! [I would report a bug
-- but things are so in flux over here that usually it turns out
-- to be my fault...].
--- #if USE_SOP
--- #error USE_SOP HERE
--- #endif

  module Control.DeepSeq.Bounded.Generic
  (

      -- * Stratified Generic Forcing

#if USE_SOP
      grnfn   ,
#else
      genericRnfn   ,
--    genericRnfnV1   ,
#endif

      -- * Pattern-based Generic Forcing

#if USE_SOP
#if USE_SOP
      grnfp   ,
#else
#if 1
      genericRnfp   ,
#if 0
      genericRnfpV1   ,
#endif
#endif
#endif
#endif

      -- * \"Molecular\" Generic Forcing

#if USE_SOP
      grnf_   ,
      gseq_   ,
      gforce_   ,
#endif

#if 0
      -- * "Control.DeepSeq" re-exports
      deepseq   ,
      force   ,
      NFData(rnf)   ,
      ($!!)   ,
#endif

  )
  where

-------------------------------------------------------------------------------

  -- In its own category, relative to GNFDataN and GNFDataP.
  -- A GHC.Generics alternative is also quite possible?
  -- Both these still require SOP instances to be derived
  -- for user data types, however, which ... well, so does
  -- the current version of Seqable (require NFDataN instances)...
#if USE_SOP
  import Control.DeepSeq.Bounded.Generic.GSeqable
#endif

  import Control.DeepSeq.Bounded.Generic.GNFDataN
#if USE_SOP
  import Control.DeepSeq.Bounded.Generic.GNFDataP
#endif

--import Control.DeepSeq.Bounded
--import Control.DeepSeq  -- needed?
--import GHC.Generics

-------------------------------------------------------------------------------

