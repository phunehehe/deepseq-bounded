
-------------------------------------------------------------------------------

{-  LANGUAGE CPP #-}

-- (Tracing code has mostly been removed for distribution.)
#define DO_TRACE 0
#define INCLUDE_SHOW_INSTANCES 0

-------------------------------------------------------------------------------

-- SOP cheat-sheet.
--
-- class SingI (Code a) => Generic a where
-- Associated Types:
--   type Code a :: [[*]]
-- Methods:
--   from :: a -> Rep a
--   to :: Rep a -> a
--
-- type Rep a = SOP I (Code a)
--
-- newtype SOP f xss = SOP (NS (NP f) xss)
--
-- data NS :: (k -> *) -> [k] -> * where
-- Constructors:
--   Z :: f x -> NS f (x : xs)	 
--   S :: NS f xs -> NS f (x : xs)	 
-- Examples:
--   Z         :: f x -> NS f (x ': xs)
--   S . Z     :: f y -> NS f (x ': y ': xs)
--   S . S . Z :: f z -> NS f (x ': y ': z ': xs)
--
-- data NP :: (k -> *) -> [k] -> * where
-- Constructors:
--   Nil :: NP f []	 
--   (:*) :: f x -> NP f xs -> NP f (x : xs) infixr 5	 
-- Examples (sic! they are correct):
--   I 'x'    :* I True  :* Nil  ::  NP I       '[ Char, Bool ]
--   K 0      :* K 1     :* Nil  ::  NP (K Int) '[ Char, Bool ]
--   Just 'x' :* Nothing :* Nil  ::  NP Maybe   '[ Char, Bool ]
--
--   > :m +Generics.SOP
--   > :m +Foo
--   > Generics.SOP.from $ G2 1 2 3
--   SOP (S (Z (I 1 :* (I 2 :* (I 3 :* Nil)))))
--   > Generics.SOP.to $ Generics.SOP.from $ G2 1 2 3 :: TG
--   G2 1 2 3

-------------------------------------------------------------------------------

  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ < 708
  {-# LANGUAGE GADTs #-}
#endif
  {-# LANGUAGE TemplateHaskell #-}

  {-# LANGUAGE BangPatterns #-}  -- only for tracing

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Generic.GNFDataP
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (uses SOP)
--
-- Support for generic deriving (via <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>) of 'NFDataP' instances.
--
-- Note that 'NFDataP' has superclasses 'NFDataN', 'NFData' and 'Typeable'.
--
-- This metaboilerplate is standard for using the generic deriving
-- facilities of "GHC.Generics" and <http://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html Generics.SOP>.
-- Consider <http://hackage.haskell.org/package/seqaid seqaid> for
-- a turnkey solution.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE GADTs #-}  -- for GHC < 7.8 (== 7.6.3)
-- > 
-- > import Generics.SOP.TH
-- > import Control.DeepSeq.Bounded ( NFDataP(..), grnfp )
-- > import Control.DeepSeq.Bounded ( NFDataN(..), grnfn )
-- > import Control.DeepSeq.Generics ( NFData(..), genericRnf )
-- > import GHC.Generics ( Generic )    -- for deriving NFData
-- > import Data.Typeable ( Typeable )  -- for name-constrained pattern nodes
-- >
-- > import Control.DeepSeq.Bounded ( forcep )
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
-- > main = return $! forcep "((!).)" (A1 (B2 undefined) A2)

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Generic.GNFDataP
  (

      grnfp

#if 0
      -- * "Control.DeepSeq" re-exports
    , deepseq
    , force
    , NFData ( rnf )
    , ($!!)
#endif

  )
  where

-------------------------------------------------------------------------------

  -- for Haddock!
--import Control.DeepSeq.Bounded.NFDataP ( NFDataP ) 
  import Control.DeepSeq.Bounded.NFDataN ( NFDataN )
  import Control.DeepSeq ( NFData )
  import Data.Typeable ( Typeable )

  import Control.DeepSeq.Bounded.NFDataP ( NFDataP, rnfp )
  import Control.DeepSeq.Bounded.NFDataN (          rnfn )
  import Control.DeepSeq                 (          rnf  )

  import Control.DeepSeq.Bounded.NFDataP ( handleAttrs )

  import Control.DeepSeq.Bounded.Pattern ( Rose(..), Pattern(..) )
  import Control.DeepSeq.Bounded.Pattern ( PatNode(..), PatNodeAttrs(..) )
  import Control.DeepSeq.Bounded.Pattern ( getPatNodeAttrs )

  import Generics.SOP

#if USE_PAR_PATNODE
  import Control.Parallel ( par )
#endif
--- #warning GNFDataP USE_PSEQ_PATNODE still to do!...
#if USE_PSEQ_PATNODE
  import Control.Parallel ( pseq )
#endif

  import Data.Maybe
  import Debug.Trace ( trace )
--import System.IO.Unsafe ( unsafePerformIO )  -- for console output only

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
  mytrace' = trace
#else
  mytrace x y = y
  mytrace' x y = y
#endif

-------------------------------------------------------------------------------

  grnfp :: forall a.
           (
             Generic a
           , HasDatatypeInfo a
--         , All Show (Map ConstructorInfo (Code a))
           , All2 NFDataP (Code a)
#if INCLUDE_SHOW_INSTANCES
           , All2 Show (Code a)
#endif
           , NFDataP a  -- NFData, NFDataN superclasses
           ) => Pattern -> a -> ()
  grnfp pat x = grnfp' dti proxy_a pat x xrep
   where
    dti = datatypeInfo proxy_a
    proxy_a = Proxy :: Proxy a
    xrep = from x

-------------------------------------------------------------------------------

  -- Show constraint is needed, but not NFData nor NFDataN.
  -- Not quite sure why, but whatever...
  grnfp' :: forall a.
            (
              Generic a
            , HasDatatypeInfo a
            , All2 NFDataP (Code a)
#if INCLUDE_SHOW_INSTANCES
            , All2 Show (Code a)
#endif
            , NFDataP a  -- NFData, NFDataN superclasses
            ) =>
                    DatatypeInfo (Code a)
                 -> Proxy a
                 -> Pattern
                 -> a
                 -> Rep a
                 -> ()
  grnfp' (ADT     _ _ cs) proxy_a pat x xrep
   = grnfpS cs         proxy_a pat x xrep
  grnfp' (Newtype _ _ c ) proxy_a pat x xrep
   = grnfpS (c :* Nil) proxy_a pat x xrep

-------------------------------------------------------------------------------

-- SOP works (here) by hitting the Z (meta)constructor for the
-- particular datatype constructor which is [inhabited].
-- i.e. This simply dials in the correct constructor.
-- Later: Looks like it does a bit more than that, now.
  grnfpS :: forall a xss.
            (
              All2 NFDataP xss
#if INCLUDE_SHOW_INSTANCES
            , All2 Show xss
#endif
            , NFDataP a  -- NFData, NFDataN superclasses
            ) =>
                    NP ConstructorInfo xss
                 -> Proxy a
                 -> Pattern
                 -> a
                 -> SOP I xss
                 -> ()

  grnfpS _ _ (Node WI{} _) _ _ = ()  -- needed!!
--grnfpS _ _ (Node WI{} _) _ (SOP (Z xs)) = ()  -- mustn't do!!
--grnfpS (m :* _) _ (Node WI{} _) _ _ = ()  -- seems not to spring the bottom

  grnfpS (m :* _) proxy_a pat@(Node pas pcs) x (SOP (Z xs))
--- | WI{} <- pas  = ()  -- too late! (SOP (Z xs)) has already forced
   | not status     = patMatchFail'' msg `seq` ()
--- | WI{} <- pas  = ()
   | handleAttrs pat x `seq` pat == Node XX []  = undefined
#if USE_PAR_PATNODE
   | doSpark as  = case pas of
       WW{} -> rnf x `par` ()
       WN{} -> rnfn n x `par` ()
       otherwise -> dorecurs
#endif
-- XXX So these TR and TI (and TW?... TN?...) also need to be up
-- in a top-level case before this one!
   | TR{} <- pas  = if elem tx treps then dorecurs else ()
#if USE_WW_DEEPSEQ
   | TI{} <- pas  = if elem tx treps then () else rnf x
   | TW{} <- pas  = if elem tx treps then rnf x else ()
#else
   | TI{} <- pas  = if elem tx treps then () else rnfn 999999 x  -- XXX thack!
#endif
--- | TI{} <- pas  = if elem tx treps then () else dorecurs
#if USE_WW_DEEPSEQ
   | WW{} <- pas  = rnf x
#endif
   | WN{} <- pas  = rnfn n x
#if USE_WW_DEEPSEQ
   | TW{} <- pas  = if elem tx treps then rnf x else ()  -- no better!
#endif
   | otherwise    = dorecurs
   where
    as = getPatNodeAttrs pas
    treps = typeConstraints as
    n = depth as
    (status,mmsg)  = grnfpP_validate True pas pcs tx xs
    !_ = mytrace ("*** "++tx) $ ()
    tx | (Constructor n) <- m  = n
       | (Infix n _ _) <- m    = n
       | (Record n _) <- m     = n
    !_ = mytrace ("VVV "++show status) $ ()
    msg = fromJust mmsg
#if USE_PAR_PATNODE
#if DO_TRACE
    dorecurs | doSpark as  = grnfpP_ m () True pas pcs xs `par` ()
             | otherwise   = grnfpP_ m () True pas pcs xs `seq` ()
#else
    dorecurs | doSpark as  = grnfpP m () True pas pcs xs `par` ()
             | otherwise   = grnfpP m () True pas pcs xs `seq` ()
#endif
#else
#if DO_TRACE
    dorecurs = grnfpP_ m () True p pcs xs `seq` ()
#else
    dorecurs = grnfpP m () True p pcs xs `seq` ()
#endif
#endif

  grnfpS (m :* ms) proxy_a pat x (SOP (S xss))
   = grnfpS ms proxy_a pat x (SOP xss)

  grnfpS _ _ _ _ _ = error "grnfpS: unexpected case!!"

-------------------------------------------------------------------------------

  grnfpP :: forall cs xs.
            (
              All NFDataP xs
#if INCLUDE_SHOW_INSTANCES
            , All Show xs
#endif
            ) =>
                    ConstructorInfo cs
                 -> ()
                 -> Bool
                 -> PatNode
                 -> [Pattern]
                 -> NP I xs
                 -> ()
  grnfpP ci acc b pp [] Nil = acc
  grnfpP ci acc True pp [] (I x :* xs)
   = grnfpP ci acc True pp [] xs
  grnfpP ci acc b pp (p@(Node pas pgcs):pcs) (I x :* xs)
   = grnfpP ci (acc `seq` step) False pp pcs xs
   where
    step | WI{} <- pas  = ()
         | TR{} <- pas  = if elem tx treps then thestep else ()
#if USE_WW_DEEPSEQ
         | TI{} <- pas  = if elem tx treps then () else rnf x
         | TW{} <- pas  = if elem tx treps then rnf x else ()
#else
         | TI{} <- pas  = if elem tx treps then () else rnfn 999999 x  -- XXX thack!
#endif
---      | TI{} <- pas  = if elem tx treps then () else thestep
         | otherwise    = thestep
     where
      as = getPatNodeAttrs pas
      treps = typeConstraints as
      thestep = rnfp p x
      tx | (Constructor n) <- ci  = n
         | (Infix n _ _) <- ci    = n
         | (Record n _) <- ci     = n
  grnfpP ci acc b pp ps xs
   = error $ "*6* "++show b++" "++show pp++" "++show ps++" "++show (lenxs xs)

-------------------------------------------------------------------------------

  -- Preliminary check for arity mismatch or constructor name mismatch.
  -- Note: I don't presently know of a way to obtain the arity of a ctor
  -- from it's name as a String. Arity checking is done between the
  -- pattern and the value, but the arity of a ctor named in a pattern
  -- constraints is not available.
  grnfpP_validate :: forall xs.
            (
              All NFDataP xs
#if INCLUDE_SHOW_INSTANCES
            , All Show xs
#endif
            ) =>
                    Bool
                 -> PatNode
                 -> [Pattern]
                 -> String
                 -> NP I xs
                 -> (Bool, Maybe String)
  grnfpP_validate True  pp@(TR as) ps tx xss
   | b          = grnfpP_validate' True pp ps tx xss
   | otherwise  = (False,Just $ "<generic> TR ctor name constraint mismatch (not "++tx++")")
   where b = head cnames == tx
         cnames = typeConstraints as
#if USE_WW_DEEPSEQ
  grnfpP_validate True  pp@(TW as) ps tx xss
   | b          = grnfpP_validate' True pp ps tx xss
   | otherwise  = (False,Just $ "<generic> TW ctor [should be type!] name constraint mismatch (not "++tx++")")
   where b = head cnames == tx
         cnames = typeConstraints as
#endif
  grnfpP_validate True  pp@(TI as) ps tx xss
   | b          = grnfpP_validate' True pp ps tx xss
   | otherwise  = (False,Just $ "<generic> TI ctor [should be type!] name constraint mismatch (not "++tx++")")
   where b = head cnames == tx
         cnames = typeConstraints as
  grnfpP_validate b     pp             ps tx xss
   = grnfpP_validate' b pp ps tx xss

  grnfpP_validate' :: forall xs.
            (
              All NFDataP xs
#if INCLUDE_SHOW_INSTANCES
            , All Show xs
#endif
            ) =>
                    Bool
                 -> PatNode
                 -> [Pattern]
                 -> String
                 -> NP I xs
                 -> (Bool, Maybe String)
  grnfpP_validate' True  (WI as) []  tx xss = (True,Nothing)
  grnfpP_validate' True  (WS as) []  tx xss = (True,Nothing)
  grnfpP_validate' True  (WN as) []  tx xss = (True,Nothing)
  grnfpP_validate' True  (WW as) []  tx xss = (True,Nothing)
  grnfpP_validate' True   pp     ps  tx xss
   | WS{} <- pp  = if lenps > 0 then (False,Nothing) else (True,Nothing)
   | b           = (True,Nothing)
   | otherwise   = (False,Just "<generic> arity mismatch #1")
   where
-- oops
--  as = getPatNodeAttrs pp
--  n = depth as
    b = lenps == lenxs xss
    lenps = length ps
  grnfpP_validate' False pp []      tx Nil
   = (True,Nothing)
  grnfpP_validate' False pp (p:pcs) tx (I x :* xs)
   = grnfpP_validate False pp pcs tx xs
  grnfpP_validate' b pp ps tx xs
   = error $ " &&& " ++ show b ++ " " ++ show pp ++ " " ++ show ps
-- = error $ show b ++ " " ++ show pp ++ " " ++ show ps ++ " " ++ show (hcliftA (Proxy :: Proxy Show) (K . show . unI) xs)

-------------------------------------------------------------------------------

  lenxs :: NP I xs' -> Int
  lenxs Nil = 0
  lenxs (I x' :* xs') = 1 + lenxs xs'
  lenxs _ = error "lenxs: unexpected"

-------------------------------------------------------------------------------

#if 0
-- Works so far as it goes, but is not currently used.
-- Also, this is exactly Data.Generics.Schemes.glength
  arity :: Data a => a -> Int
  arity = length . gmapQ (const ())
#endif

-------------------------------------------------------------------------------

  patMatchFail'' :: String -> ()
  patMatchFail'' msg
#if 0
     -- With this, the expected match failures are seen.
     = error "BOO!!!!"
#else
#if WARN_PATTERN_MATCH_FAILURE
     = trace ("GNFDataP: warning: pattern match failure (" ++ msg ++ ")") $ ()
--   = unsafePerformIO $! putStrLn $! "GNFDataP: warning: pattern match failure (" ++ msg ++ ")"
#else
     = ()
#endif
#endif

-------------------------------------------------------------------------------

