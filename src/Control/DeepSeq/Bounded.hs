
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#if PROVIDE_DATA_FAMILY

  {-# LANGUAGE CPP #-}
  {-# LANGUAGE TypeFamilies #-}
  {-  LANGUAGE TypeSynonymInstances #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-  LANGUAGE DatatypeContexts #-}

#endif

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable (but GHC for a few modules using generics)
--
-- We provide forcing functions which take a non-strict value
-- of a datatype, and force its evaluation in a pricipled way.
-- As with 'NFData', one should bear in mind the difference
-- between forcing and demand: in order for your forcing to
-- take effect, demand must be placed on the forcing function
-- itself by the course of execution.
--
-- Artificial forcing is most commonly used to suppress space
-- leak, but it has many uses besides.
-- Another is to ensure that exceptions hidden within lazy
-- fields of a data structure don't leak outside the scope of the
-- exception handler; another is to force evaluation of a data structure in
-- one thread, before passing it to another thread (preventing work moving
-- to the wrong threads, a form of /time leak/).
--
-- Unlike <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html DeepSeq>,
-- potentially infinite values of coinductive data types
-- are supported by principled bounding of deep evaluation.
--
-- Refer to comments in the <http://hackage.haskell.org/package/deepseq deepseq> package for more information on how
-- artificial forcing can be useful.
--
-- Recently, control (static or dynamic) of parallelisation has been added.
-- Control of evaluation order is also supported (via <http://hackage.haskell.org/package/parallel/docs/Control-Parallel.html#v:pseq pseq>).

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded
  (

     -- * Forced evaluation to an arbitrary finite depth

       module Control.DeepSeq.Bounded.NFDataN

     -- * Forced evaluation over a pattern (arbitrary finite, or dynamic)

     , module Control.DeepSeq.Bounded.Pattern
     , module Control.DeepSeq.Bounded.Compile
     , module Control.DeepSeq.Bounded.PatUtil      -- re-exports the former
     , module Control.DeepSeq.Bounded.NFDataP     -- re-exports both
#if USE_SOP
     , module Control.DeepSeq.Bounded.NFDataPDyn  -- re-exps. all 3 above
#endif

     -- XXX On second thoughts, I don't want to depend on containers
     -- package just to provide a rose tree data type!...
     -- I don't export Data.Tree; ideally the API would never
     -- require the user to work at that low level, but if
     -- necessary they can import Data.Tree themselves.

     , Rose(..)

     -- * Forced evaluation "molecular style"

     , module Control.DeepSeq.Bounded.Seqable

#if ! HASKELL98_FRAGMENT
     -- * Generic deriving support via "Generics.SOP"

#if USE_SOP
     , module Control.DeepSeq.Bounded.Generic.GSeqable
#endif

     , module Control.DeepSeq.Bounded.Generic.GNFDataN
#if USE_SOP
     , module Control.DeepSeq.Bounded.Generic.GNFDataP
#endif
#endif

#if USE_WW_DEEPSEQ
     -- * Re-exported

     , module Control.DeepSeq
#endif

-- (I'm not sure what the exact bound is, but 7.6.3 is too low.)
#if __GLASGOW_HASKELL__ >= 708
#if PROVIDE_DATA_FAMILY
     -- * \"I can has data family?...\"

     , FF(..)
#endif
#endif

  )
  where

-------------------------------------------------------------------------------

#if PROVIDE_DATA_FAMILY
  -- Remarkable, this is not already in scope by re-export!
  import Generics.SOP ( Generic )
#endif

  import Control.DeepSeq.Bounded.Seqable

  import Control.DeepSeq.Bounded.NFDataN

  import Control.DeepSeq.Bounded.Pattern
  import Control.DeepSeq.Bounded.Compile
  import Control.DeepSeq.Bounded.PatUtil
  import Control.DeepSeq.Bounded.NFDataP
#if USE_SOP
  import Control.DeepSeq.Bounded.NFDataPDyn
#endif

#if ! HASKELL98_FRAGMENT
  -- In its own category, relative to GNFDataN and GNFDataP.
#if USE_SOP
  import Control.DeepSeq.Bounded.Generic.GSeqable
#endif

  import Control.DeepSeq.Bounded.Generic.GNFDataN
#if USE_SOP
  import Control.DeepSeq.Bounded.Generic.GNFDataP
#endif
#endif

#if USE_WW_DEEPSEQ
  import Control.DeepSeq
#endif

-------------------------------------------------------------------------------

-- Lest I forget WHY -- now, you can express
--
--   "composition of partially-applied forcing functions commutes"
--
-- directly in Haskell (and compute with it, etc)!...

-- (I'm not sure what the exact bound is, but 7.6.3 is too low.)
#if __GLASGOW_HASKELL__ >= 708

#if PROVIDE_DATA_FAMILY

-- I do believe this came out as a nice first attempt
-- at using any type extensions! After 25 years of
-- functional programming, and 15 in Haskell, I've
-- never used anything non-H98 in my own code, unless
-- a library I used forced me to.  Never in my own code!
-- Until today... / I can see why people get excited
-- about this stuff, it's definitely "elegant"!

  class FF k where
    data F k :: * -> *
    -- | The constraints could probably shrink to just 'Generic' with
    -- a bit of work...  In fact, trying it now -- to do away with NFDataN
    -- and NFDataP classes, and just "alias the SOP generic functions",
    -- as did in Seqable... [Fail. for now]
    phi :: (
#if JUST_ALIAS_GSEQABLE
               Generic v
#else
               Seqable v
#endif
             , NFDataN v
             , NFDataP v
           ) => k -> F k v -> v

  instance FF SeqNode where            -- Seqable[/Generic]
    data F SeqNode v = FSeqNode v
    phi k (FSeqNode m) = force_ k m

  instance FF Int where                -- NFDataN
    data F Int v = FInt v
    phi k (FInt m) = forcen k m

  instance FF Pattern where            -- NFDataP
    data F Pattern v = FPattern v
#if OVERLOADED_STRINGS
    phi k (FPattern m) = forcep k m
#else
    phi k (FPattern m) = forcep_ k m
#endif

#if 0
  instance (FF Pattern, FF a) => FF (Pattern,a) where
    data F (Pattern, a) = FPattern v
    phi (pat,a) (FPattern m) = forcep k m
#endif
    
#endif

#endif

-------------------------------------------------------------------------------

