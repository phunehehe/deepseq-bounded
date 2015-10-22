
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#define DO_TRACE 0
#define WARN_IGNORED_SUBPATTERNS 1
#define NEVER_IGNORE_SUBPATTERNS 0
#define USE_GENERICQ 0

-- Now specified via --flag=[-]USE_WWW_DEEPSEQ
--- #define USE_WW_DEEPSEQ 1

-------------------------------------------------------------------------------

#if USE_SOP
  -- XXX and what happened to DataKinds??... (only when also TemplateHaskell?)
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ < 708
  {-# LANGUAGE GADTs #-}
#endif
#endif

  {-# LANGUAGE Rank2Types #-}
  {-  LANGUAGE ScopedTypeVariables #-}

  -- For tracing only:
  {-  LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.NFDataPDyn
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.NFDataPDyn
  (

     -- * Dynamic pattern-directed forcing

     -- | These functions are concerned with extending the forcing pattern
     -- dynamically, depending on the types of the nodes encountered while
     -- generically traversing a term.
     --
     -- (Work in progress...).

       rnfpDyn
     , deepseqpDyn
     , forcepDyn

     , rnfpDyn'
     , deepseqpDyn'
     , forcepDyn'

     , rnfpDyn''
     , deepseqpDyn''
     , forcepDyn''

     -- * Re-exported for convenience

     , module Control.DeepSeq.Bounded.NFDataP

  )
  where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded.NFDataP

  import Control.DeepSeq.Bounded.NFDataN  -- finally used ("*3" etc.)

#if USE_WW_DEEPSEQ
  import Control.DeepSeq ( NFData )
--import Control.DeepSeq ( rnf )
#endif

  import Data.Data ( Data )
#if USE_GENERICQ
  import Data.Generics ( GenericQ )
#endif

#if 1
  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )
#else
-- XXX These are NOT interchangeable!
#if __GLASGOW_HASKELL__ >= 781
  import Data.Typeable ( typeRep )
#else
  import Data.Typeable ( typeOf )
#endif
#endif

  import Debug.Trace ( trace )

#if USE_SOP
  import Generics.SOP hiding ( Shape )
#endif

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
#else
  mytrace _ = id
#endif

-------------------------------------------------------------------------------

  -- | SOP/SYB hybrid dynamic 'deepseqp'.
  deepseqpDyn :: forall a b. (Show a, NFDataP a, Generic a, Data a
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
    , All2 Data (Code a)
#if USE_GENERICQ
    ) => GenericQ PatNode -> a -> b -> b
#else
    ) => (forall c. Data c => c -> PatNode) -> a -> b -> b
#endif
  deepseqpDyn fg a b = rnfpDyn fg a `seq` b
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-  RULES
    "deepseqpDyn/composition"    forall fg1 fg2 x.  (.) (deepseqpDyn fg2) (deepseqpDyn fg1) x = deepseqpDyn (gcombQ fg1 fg2) x
      #-}

  -- | SOP/Typeable hybrid dynamic 'deepseqp'.
  deepseqpDyn'' :: forall a b. (Show a, NFDataP a, Generic a, Data a
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
--  , All2 Data (Code a)
    ) => (forall c. Typeable c => c -> PatNode) -> a -> b -> b
  deepseqpDyn'' fg a b = rnfpDyn'' fg a `seq` b
  -- XXX Partially-applied; is that okay in GHC RULES?
  {-  RULES
    "deepseqpDyn''/composition"    forall fg1 fg2 x.  (.) (deepseqpDyn'' fg2) (deepseqpDyn'' fg1) x = deepseqpDyn'' (gcombQ fg1 fg2) x
      #-}

  -- | SOP-only dynamic 'deepseqp'.
  deepseqpDyn' :: forall a b. (Show a, NFDataP a, Generic a, Data a
    , All2 Generic (Code a)
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
    ) => (forall c. Generic c => c -> PatNode) -> a -> b -> b
  deepseqpDyn' fg a b = rnfpDyn' fg a `seq` b

-------------------------------------------------------------------------------

  -- | SOP/SYB hybrid dynamic 'forcep'.
  forcepDyn :: forall a. (Show a, NFDataP a, Generic a, Data a
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
    , All2 Data (Code a)
#if USE_GENERICQ
    ) => GenericQ PatNode -> a -> a
#else
    ) => (forall c. Data c => c -> PatNode) -> a -> a
#endif
  forcepDyn fg x = deepseqpDyn fg x x
  {-  RULES
    "forcepDyn/composition"    forall fg1 fg2 x.  (.) (forcepDyn fg2) (forcepDyn fg1) x = forcepDyn (gcombQ fg1 fg2) x
      #-}

  -- | SOP/Typeable hybrid dynamic 'forcep'.
  forcepDyn'' :: forall a. (Show a, NFDataP a, Generic a, Data a
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
--  , All2 Data (Code a)
    ) => (forall c. Typeable c => c -> PatNode) -> a -> a
  forcepDyn'' fg x = deepseqpDyn'' fg x x
  {-  RULES
    "forcepDyn''/composition"    forall fg1 fg2 x.  (.) (forcepDyn'' fg2) (forcepDyn'' fg1) x = forcepDyn'' (gcombQ fg1 fg2) x
      #-}

  -- | SOP-only dynamic 'forcep'.
  forcepDyn' :: forall a. (Show a, NFDataP a, Generic a, Data a
    , All2 Generic (Code a)
    , All2 Show (Code a)
    , All2 NFData (Code a)
    , All2 NFDataN (Code a)
    , All2 NFDataP (Code a)
    ) => (forall c. Generic c => c -> PatNode) -> a -> a
  forcepDyn' fg x = deepseqpDyn' fg x x

-------------------------------------------------------------------------------

#if USE_SOP

-------------------------------------------------------------------------------

#if 1

-------------------------------------------------------------------------------

-- This one, trying for a SOP (not SYB) based generic stop function arg.
-- This should be straightforward. We have adequate precedent in GNFDataP.

  -- | SOP-only dynamic 'rnfp'.
  -- Takes a SOP generic function yielding 'PatNode', which extends
  -- the pattern dynamically, depending on the type of the value node.
  rnfpDyn' :: forall a. (
               Generic a
             , All2 Generic (Code a)
             , All2 Show    (Code a)
             , All2 NFData  (Code a)
             , All2 NFDataN (Code a)
             , All2 NFDataP (Code a)
--           , NFData  a
--           , NFDataN a
--           , NFDataP a
             ) =>
                     ( forall c. (
                         Generic c
--                     , All2 Show (Code c)
                       ) => c -> PatNode
                     )
                  -> a
                  -> ()
  rnfpDyn' fg d = rnfpDynS' fg (from d)

  rnfpDynS' :: forall xss. (
                All2 Generic xss
              , All2 Show    xss
              , All2 NFData  xss
              , All2 NFDataN xss
              , All2 NFDataP xss
--            , NFData  xss
--            , NFDataN xss
--            , NFDataP xss
              ) =>
                      ( forall c. (
                          Generic c
--                      , All2 Show (Code c)
                        ) => c -> PatNode
                      )
                   -> SOP I xss
                   -> ()
  rnfpDynS' fg (SOP (Z xs))  = rnfpDynP' fg xs
  rnfpDynS' fg (SOP (S xss)) = rnfpDynS' fg (SOP xss)

  rnfpDynP' :: forall xs. (
                All Generic xs
              , All Show    xs
              , All NFData  xs
              , All NFDataN xs
              , All NFDataP xs
--            , NFData  xs
--            , NFDataN xs
--            , NFDataP xs
              ) =>
                      ( forall c. (
                          Generic c
--                      , All2 Show (Code c)
                        ) => c -> PatNode
                      )
                   -> NP I xs
                   -> ()
  rnfpDynP' fg Nil         = ()
  rnfpDynP' fg (I x :* xs)
   | trace (show $ typeOf x) False = undefined
   | WI{} <- pn  = trace ("Boo A "        ) $ rnfpDynP' fg xs         `seq` ()
   | otherwise   = trace ("Boo B "++show x) $ rnfpDynP' fg xs `seq` x `seq` ()
   where
--  pn = WR emptyPatNodeAttrs
    pn = fg x {-:: PatNode-}
    pat = Node pn [] {-:: Pattern-}

-------------------------------------------------------------------------------

-- Trying explicit SOP recursion, since it seems then Proxy isn't needed?

  -- | SOP/SYB hybrid dynamic 'rnfp'.
  -- Takes a SYB 'GenericQ' 'PatNode' argument, which extends the pattern
  -- dynamically, depending on the type of the value node.
---rnfpDyn :: (Generic a, All2 NFData (Code a)) => a -> ()
  rnfpDyn :: forall a. (
               Generic a
             , All2 Show    (Code a)
             , All2 NFData  (Code a)
             , All2 NFDataN (Code a)
             , All2 NFDataP (Code a)
             , All2 Data    (Code a)
--           , NFData  a
--           , NFDataN a
--           , NFDataP a
             ) =>
#if 0
                     a
#else
#if USE_GENERICQ
                     GenericQ PatNode
#else
                     ( forall c. (
#if 1
                         Data c
#else
                         Generic c
--                     , All2 Show (Code c)
#endif
                       ) => c -> PatNode
                     )
#endif
                  -> a
#endif
                  -> ()
#if 0
  rnfpDyn d = ()
#else
  rnfpDyn fg d = rnfpDynS fg (from d)
--rnfpDyn fg d = ()
#endif

--rnfpDynS :: (All2 NFData xss) => SOP I xss -> ()
  rnfpDynS :: forall xss. (
                All2 Show    xss
              , All2 NFData  xss
              , All2 NFDataN xss
              , All2 NFDataP xss
              , All2 Data    xss
--            , NFData  xss
--            , NFDataN xss
--            , NFDataP xss
              ) =>
#if 0
                      SOP I xss
#else
#if USE_GENERICQ
                     GenericQ PatNode
#else
                      ( forall c. (
                          Data c
                        ) => c -> PatNode
                      )
#endif
                   -> SOP I xss
#endif
                   -> ()
  rnfpDynS fg (SOP (Z xs))  = rnfpDynP fg xs
  rnfpDynS fg (SOP (S xss)) = rnfpDynS fg (SOP xss)

--rnfpDynP :: (All NFData xs) => NP I xs -> ()
  rnfpDynP :: forall xs. (
                All Show    xs
              , All NFData  xs
              , All NFDataN xs
              , All NFDataP xs
              , All Data    xs
--            , NFData  xs
--            , NFDataN xs
--            , NFDataP xs
              ) =>
#if 0
                      NP I xs
#else
#if USE_GENERICQ
                     GenericQ PatNode
#else
                      ( forall c. (
                          Data c
                        ) => c -> PatNode
                      )
#endif
                   -> NP I xs
#endif
                   -> ()
  rnfpDynP fg Nil         = ()
--rnfpDynP fg (I x :* xs) = rnfpDynP fg xs `seq` x `seq` ()
  rnfpDynP fg (I x :* xs)
   | trace (show $ typeOf x) False = undefined
--- | trace (show $ typeOf x) $! False = undefined
#if 0
   | WI{} <- pn  = trace ("AAA "++show x) $ ()
   | otherwise   = trace ("BBB "++show x) $ ()
#else
   | WI{} <- pn  = trace ("AAA "        ) $ rnfpDynP fg xs         `seq` ()
--- | WI{} <- pn  = trace ("AAA "++show x) $ rnfpDynP fg xs         `seq` ()
--- | WI{} <- pn  = trace ("AAA "++show x) $ ()
--- | otherwise  = trace ("BBB "++show x) $ rnfpDynP fg xs `seq` rnfp pat x `seq` ()
---- | otherwise  = trace ("BBB "++show x) $ rnfpDynP fg xs `seq` rnfpDyn fg x `seq` ()
   | otherwise  = trace ("BBB "++show x) $ rnfpDynP fg xs `seq` x `seq` ()
#endif
   where
#if 0
    pn = WR emptyPatNodeAttrs
#else
    pn = trace (show $ fg x) $ fg x {-:: PatNode-}
--  pn = fg x {-:: PatNode-}
#endif
    pat = Node pn [] {-:: Pattern-}
#if 0
    proxy_a = Proxy :: Proxy a
--  proxy_a = Proxy (Proxy :: Proxy a, Proxy :: Proxy b)
#endif
--rnfpDynP (I x :* xs) = x `deepseq` (rnfpDynP xs)

-------------------------------------------------------------------------------

  -- | SOP/Typeable hybrid dynamic 'rnfp'.
  -- Takes a SYB 'GenericQ' 'PatNode' argument, which extends the pattern
  -- dynamically, depending on the type of the value node.
  rnfpDyn'' :: forall a. (
               Generic a
             , All2 Show    (Code a)
             , All2 NFData  (Code a)
             , All2 NFDataN (Code a)
             , All2 NFDataP (Code a)
--           , All2 Data    (Code a)
             ) =>
                     ( forall c. (
                         Typeable c
                       ) => c -> PatNode
                     )
                  -> a
                  -> ()
  rnfpDyn'' fg d = rnfpDyn''S fg (from d)

  rnfpDyn''S :: forall xss. (
                All2 Show    xss
              , All2 NFData  xss
              , All2 NFDataN xss
              , All2 NFDataP xss
--            , All2 Data    xss
              ) =>
                      ( forall c. (
                          Typeable c
                        ) => c -> PatNode
                      )
                   -> SOP I xss
                   -> ()
  rnfpDyn''S fg (SOP (Z xs))  = rnfpDyn''P fg xs
  rnfpDyn''S fg (SOP (S xss)) = rnfpDyn''S fg (SOP xss)

  rnfpDyn''P :: forall xs. (
                All Show    xs
              , All NFData  xs
              , All NFDataN xs
              , All NFDataP xs
--            , All Data    xs
              ) =>
                      ( forall c. (
                          Typeable c
                        ) => c -> PatNode
                      )
                   -> NP I xs
                   -> ()
  rnfpDyn''P fg Nil         = ()
  rnfpDyn''P fg (I x :* xs)
   | trace (show $ typeOf x) False = undefined
   | WI{} <- pn  = trace ("AAA "        ) $ rnfpDyn''P fg xs         `seq` ()
   | otherwise   = trace ("BBB "++show x) $ rnfpDyn''P fg xs `seq` x `seq` ()
   where
    pn = trace (show $ fg x) $ fg x {-:: PatNode-}
--  pn = fg x {-:: PatNode-}
    pat = Node pn [] {-:: Pattern-}

-------------------------------------------------------------------------------

#else

#if 0
               Generic a
             , HasDatatypeInfo a
--           , All Show (Map ConstructorInfo (Code a))
             , All2 NFDataP (Code a)
             , All2 Show (Code a)
             , Typeable a
             , NFDataN a
             , NFDataP a
#endif
  rnfpDyn :: forall a. (
               Generic a
             , All2 Show    (Code a)
             , All2 NFDataP (Code a)
             , All2 NFData  (Code a)
             , NFDataP a
             , NFData  a
             ) =>
#if 1
                     a
#else
                     ( forall b. (
                         Generic b
                       , All2 Show    (Code b)
                       ) => b -> PatNode
                     )
                  -> a
#endif
                  -> ()
--rnfpDyn fg d = ()
#if 1
  rnfpDyn d
#else
  rnfpDyn fg d
#endif
   -- This appears to work from the bottom-up which is not
   -- what's wanted ... but I must be mistaken? Well, not "must"...
   -- The question is whether hcollapse makes a head available
   -- without having to finish recursions completely.
#if 1
   = (rnfp pat . hcollapse . hcliftA (Proxy :: Proxy a) (\ (I x) -> K (rnfpDyn x)) . from) d
-- = (rnfp pat . hcollapse . hcliftA (Proxy :: Proxy NFDataP) (\ (I x) -> K (rnfpDyn x)) . from) d
#else
   = (rnfp pat . hcollapse . hcliftA (Proxy :: Proxy NFDataP) (\ (I x) -> K (rnfpDyn fg x)) . from) d
#endif
-- = (rnfp pat . hcollapse . hcliftA (Proxy :: Proxy a) (\ (I x) -> K (rnfpDyn fg x)) . from) d
-- = (rnfp pat . hcollapse . hcliftA proxy_a (\ (I x) -> K (rnfpDyn fg x)) . from) d
-- = rnfp pat x `seq` map (rnfpDyn fg) x `seq` ()
   where
#if 1
    pn = WR emptyPatNodeAttrs
--  pn = WR
#else
    pn = fg d {-:: PatNode-}
#endif
    pat = Node pn [] {-:: Pattern-}
    proxy_a = Proxy :: Proxy a
--  proxy_a = Proxy (Proxy :: Proxy a, Proxy :: Proxy b)

#endif

-------------------------------------------------------------------------------

#else

-- XXX I guess, at the very least, you'll need
-- syb-with-class if you want to implement this...

#error Cannot use NFDataPDyn without flag USE_SOP being True (at least at this time).

#endif

-------------------------------------------------------------------------------

