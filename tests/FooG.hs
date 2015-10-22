
-- XXX This file could be cleaned up a lot, but that's
-- not quite a priority at this moment...

  {-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------

#define USE_TRACE 1
-- This so can get an honest comparison for the user-defined datatypes;
-- if this is 0, the NFDataN instances will be derived via GHC.Generics.
-- (The NFData instances are derived in any case.)
#define USE_MANUAL_INSTANCES 0

-------------------------------------------------------------------------------

#if USE_SOP
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  -- XXX This wasn't here, when came to make the edit to add the CPP below it!
  {-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ < 708
  {-# LANGUAGE GADTs #-}
#endif
  {-# LANGUAGE TemplateHaskell #-}
#else
  {-# LANGUAGE DeriveGeneric #-}
#endif

  {-# LANGUAGE DeriveGeneric #-}  -- still needed to derive NFData...

  {-# LANGUAGE DeriveDataTypeable #-}  -- to make BottomedOut

-------------------------------------------------------------------------------

  module FooG where

-------------------------------------------------------------------------------

--import Control.DeepSeq.Generics
  import Control.DeepSeq.Bounded hiding ( F )
  import Control.DeepSeq.Bounded.Generic

#if USE_SOP
  import Generics.SOP.TH
#else
  import GHC.Generics
--import GHC.Generics ( Generic )
#endif

  import GHC.Generics ( Generic ) -- still needed to derive NFData...
  import Control.DeepSeq.Generics

  import Data.Maybe

  import Control.Exception
--import Control.Monad ( guard )
  import Data.Typeable ( Typeable )
  import Data.Typeable ( typeOf )

--import Util
  import Debug.Trace ( trace )
  import Control.DeepSeq

  import Bottom

  import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

  data TA = A1 Bool Int
          | A4 (Float,TB,Int)  -- (testing reordering!)
          | A2 Int
          | A3 TB Bool
          | A5 (Int,Float)
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TA where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TA where
    rnfn n (A1 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
    rnfn n (A2 x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
    rnfn n (A3 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
    rnfn n (A4 x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
#else
#if USE_SOP
  instance NFDataN TA where rnfn = grnfn
#else
  instance NFDataN TA where rnfn = genericRnfn
#endif
#endif
  expTA = A1 __ 3

  data TB = B1 Bool TA
          | B2 TA TB
          | B3 Bool TA Int
          | B4 Bool Int TA
          | B5 Bool Int Float
          | B6 TA
          | B7 TB
          | B8 (Int,Float)
          | B9 Bool (Int,Float) Int
          | B10 (Int,Int,(Int,Int,Int,Int),Int)
          | B11 Bool (Int,Int,(Int,Int,Int,Int),Int)
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TB where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TB where
    rnfn n (B1 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
    rnfn n (B2 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
    rnfn n (B3 x y z)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y `seq` rnfn n' z
    rnfn n (B4 x y z)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y `seq` rnfn n' z
    rnfn n (B5 x y z)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y `seq` rnfn n' z
    rnfn n (B6 x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
#else
#if USE_SOP
  instance NFDataN TB where rnfn = grnfn
#else
  instance NFDataN TB where rnfn = genericRnfn
#endif
#endif

#if USE_SOP
  deriveGeneric ''TA
  deriveGeneric ''TB
#endif

  expTB_1 = B2 (A1 True 4) (B1 True (A2 23))
--expTB_1 = B2 (A1 True 4) (B1 True (A2 __))
  expTB_2 = B2 (A1 __   4) (B1 True (A2 __))
  expTB_3 = B2 (A1 __   4) (B1 __   (A2 __))
--expTB_4 = B2 (A1 True 4) (B1 True (A2 __))
--expTB_4 = B2 (A1 True 4) (B1 __   (A2 __))
  expTB_4 = B2 (A1 True 4) (B1 __   (A2 23))
  expTB_5
   = B2
        (A3 (B1 False (A2 __)) True)
        (B1 True (A1 False 4))
  expTB_6
   = B2
        (A3 (B1 __ (A2 3)) True)
        (B1 True (A1 False 4))
  expTB_7
   = B2
        (A3 (B1 False (A2 3)) True)
        (B1 __ (A1 False 4))
  expTB_8
   = B2
        (A3 (B1 False (A2 3)) True)
        (B1 True (A1 __ 4))
  expTB_9
   = B2
        (A3 (B3 False __ 5) True)
        (B1 True (A1 False 4))
  expTB_10                                                -- 5  .
   = B2
        (A3 (B3 False (A2 3) 5) True)
        (B1 True (A4 (2.3, B1 False (A1 True 4), __)))
  expTB_11
   = B2
        (A3 (B3 False (A2 3) 5) True)
        (B1 True (A4 (__, B1 False (A1 True 4), 6)))
  expTB_12
   = B2
        (A3 (B3 False (A2 3) 5) True)
        (B1 True (A4 (2.3, B1 False (A1 __ 4), 6)))
  expTB_13
   = B2
        (A3 (B3 False (A2 3) __) True)
        (B1 True (A1 False 4))
  expTB_14
   = B2
        (A3 (B3 False (A2 __) 5) True)
        (B1 True (A1 False 4))
  expTB_15 = A3 (B3 False __ 5) False  -- not expTB actually
                             -- For the non-combinator SOP recursion version:
  expTB_16 = B3 False (A2 __) 5   -- E/A =  3  .   B3 Bool TA Int
--expTB_16 = B7 (B3 False __ 5)   -- E/A =  3  .   B3 Bool TA Int
--expTB_16 = B3 False __ 5        -- E/A =  2  .   B3 Bool TA Int
  expTB_17 = B4 False 5 __        -- E/A =  2  .   B4 Bool Int TA
  expTB_18 = B5 False 5 __        -- E/A =  2  .   B5 Bool Int Float
  expTB_19 = B1 False __          -- E/A =  2  .   B1 Bool TA
  expTB_20 = B6 __                -- E/A =  2  .   B6 TA

--data TA = A1 Bool Int
--        | A4 (Float,TB,Int)
--        | A2 Int
--        | A3 TB Bool
--        | A5 (Int,Float)
--data TB = B1 Bool TA
--        | B2 TA TB
--        | B3 Bool TA Int
--        | B4 Bool Int TA
--        | B5 Bool Int Float
--        | B6 TA
--        | B7 TB
--        | B8 (Int,Float)
--        | B9 Bool (Int,Float) Int
  expTB_21 = B7 (B7 (B5 __ 5 2.3))    -- E/A =  4  .
  expTB_22 = B7 (B7 (B5 True 5 __))   -- E/A =  4  .
  expTB_23 = B1
                True
                (A4
                    ( 2.3
                    , B2
                         -- forcen 4 takes you to here (unwrapping B2).
                                                     -- Bottoms-out at n=?
                                                     -- (X Y) = SOP recurs.
                                                     -- E(xpect) A(ctual)
                                                     -- E  A (. if A=E)
--                       (A3 (B6 (A2 __)) False)     -- 8  .
                         (A3 (B6 (A2 7)) False)      -- never
                         (B3 True (A2 5) __)         -- 6  8  ( 6  . )
--                       (B9 True (2,__) 6)          -- 7  .  ( 7  . )
--                       (B9 True (__,3.4) 6)        -- 7  .  ( 7  . )
--                       (B9 True __ 6)              -- 6  7  ( 6  . )
--                       (B3 True __ 6)              -- 6  7  ( 6  . )
--                       (B3 True (A2 __) 6)         -- 7  .  ( 7  . )
--                       (B3 __ (A2 5) 6)            -- 6  .  ( 6  . )
--                       (B3 True (A2 5) 6)          -- never
                    , 7))
                                                           -- E  A
--expTB_24 = B10 (1,2,(3,__,5,6),7)                        -- 4  .
  expTB_24 = B11 False (1,2,(3,__,5,6),7)                  -- 4  .
  expTB_25 = B9 True __ 6                                  -- 2  .
--expTB_25 = B9 __ (1,2.3) 6                               -- 2  .
--expTB_26 = B7 (B9 True __ 6)                             -- 3  .
--expTB_26 = B7 (B7 (B7 (B7 (B9 True __ 6))))              -- 6  .
  expTB_26 = B2 (A2 8) (B7 (B7 (B7 (B7 (B9 True __ 6)))))  -- 7  .

  getA (A3 _ b) = show b

  expBase7 = expTB_1
  expBase8 = expTB_2
  expBase9 = expTB_3
  expBase10 = expTB_4
  expBase11 = expTB_5
  expBase12 = expTB_6
  expBase13 = expTB_7
  expBase14 = expTB_8
  expBase15 = expTB_9
  expBase16 = expTB_10
  expBase17 = expTB_11
  expBase18 = expTB_12
  expBase19 = expTB_13
  expBase20 = expTB_14
  expBase21 = expTB_15
  expBase22 = expTB_16
  expBase23 = expTB_17
  expBase24 = expTB_18
  expBase25 = expTB_19
  expBase26 = expTB_20

  expBase27 = expTB_21
  expBase28 = expTB_22
  expBase29 = expTB_23
  expBase30 = expTB_24
  expBase31 = expTB_25
  expBase32 = expTB_26

-------------------------------------------------------------------------------

#if 1
  getB_1 :: TB -> String
  getB_1 (B1 b _) = show b
  getB_1 (B2 _ (B1 b _)) = show b
  getB_1 (B3 _ _ n) = show n
  getB_1 (B4 _ n _) = show n
  getB_1 (B5 _ n _) = show n
  getB_1 (B6 _) = "beesix"
--getB_1 (B7 x) = getB_1 x
  getB_1 _ = error "!"
  getB_2 :: TB -> String
  getB_2 (B1 b _) = show b
  getB_2 (B2 (A1 _ n) _) = show n
  getB_2 (B2 (A3 _ b) _) = show b
  getB_2 (B3 _ _ n) = show n
  getB_2 (B4 _ n _) = show n
  getB_2 (B5 _ n _) = show n
  getB_2 (B6 _) = "beesix"
  getB_2 _ = error "!"
  getB_3 _ = "<bah>"
#else
  getB_1 :: TB -> Bool
  getB_1 (B2 _ (B1 b _)) = b
  getB_1 _ = error "!"
  getB_2 :: TB -> Int
  getB_2 (B2 (A1 _ n) _) = n
  getB_2 _ = error "!"
#endif

-------------------------------------------------------------------------------

  data TC = C Int Bool
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TC where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TC where
    rnfn n (C x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
#else
#if USE_SOP
  deriveGeneric ''TC
  instance NFDataN TC where rnfn = grnfn
#else
  instance NFDataN TC where rnfn = genericRnfn
#endif
#endif
  expTC = C __ True
  getC (C _ x) = x

  data TD = D1 Int Bool
          | D2 Int Bool
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TD where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TD where
    rnfn n (D1 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
    rnfn n (D2 x y)
     | n <= 0     = ()
     | otherwise  = let n' = -1+n in rnfn n' x `seq` rnfn n' y
#else
#if USE_SOP
  deriveGeneric ''TD
  instance NFDataN TD where rnfn = grnfn
#else
  instance NFDataN TD where rnfn = genericRnfn
#endif
#endif
  expTD_1 = D1 __ True
  expTD_2 = D2 __ True
  getD (D1 _ x) = x
  getD (D2 _ x) = x

-------------------------------------------------------------------------------

  data TE = E TF
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TE where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TE where
    rnfn n (E x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
#else
#if USE_SOP
  instance NFDataN TE where rnfn = grnfn
#else
  instance NFDataN TE where rnfn = genericRnfn
#endif
#endif

  data TF = F Int
#if USE_SOP
    deriving (Show)
#else
    deriving (Show,Generic)
#endif
--instance NFData TF where rnf = genericRnf
#if USE_MANUAL_INSTANCES
  instance NFDataN TF where
    rnfn n (F x)
     | n <= 0     = ()
     | otherwise  = rnfn (-1+n) x
#else
#if USE_SOP
  instance NFDataN TF where rnfn = grnfn
#else
  instance NFDataN TF where rnfn = genericRnfn
#endif
#endif

#if USE_SOP
  deriveGeneric ''TE
  deriveGeneric ''TF
#endif

  expTE_1 = E (F 23)
  expTE_2 = E (F __)
  expTE_3 = E __

#if 1
  -- These all produce the same, unexpected result; so maybe the
  -- pattern-matching isn't implicated?...
  getE (E _) = "getee"
--getE (E ~_) = "getee"
--getE (E ~x) = "getee"
#else
  -- In order for this not to just print "getee" every time,
  -- you need to make "get $ " to "get $! " everywhere it
  -- occurs in Main.hs.
  getE _ = "getee"  -- making sure the (E _) isn't triggering...
#endif

-------------------------------------------------------------------------------

#if USE_SOP

  -- Following the shape of the base-typed test expression
  -- used in Blah.doit10:  (_, [_,_,_], _) :: (Float, [Int], Bool)

  data TG = G1 | G2 Int Int Int
    deriving (Show,Generic,Typeable)
--  deriving (Show,Generic)
--  deriving (Show)
--instance NFData TG where rnf = genericRnf
  instance NFDataP TG where rnfp = grnfp
  instance NFDataN TG where rnfn = grnfn
  instance NFData TG where rnf = genericRnf

  deriveGeneric ''TG

  expTG_1 = ( G1, G2 5 6 7, True )
  expTG_2 = ( __, G2 __ __ 7, __ )
  expTG_3 = ( G1, G2 __ __ 7, __ )
  expTG_4 = ( G1, G2 5 6 7, __ )
  expTG_5 = ( G1, G2 5 __ 7, __ )   -- this is the analogue of doit10 exp

  getG :: (TG,TG,Bool) -> String
  getG (_,(G2 _ _ n),_) = show n
--getG (_,(G2 _ _ _),_) = "getG-G2"
  getG _ = "getG-!"

  expTG_6 = G2 5 __ 7
  getG' :: TG -> String
  getG' (G2 _ _ n) = show n

#endif

-------------------------------------------------------------------------------

#if USE_SOP

  -- A more elaborate shape for more thorough testing!

  data TH = H1 Float | H2 Int [TH] Bool | H3 | H4 (TH,TI)
    deriving (Show,Generic,Typeable)
  instance NFDataP TH where rnfp = grnfp
  instance NFDataN TH where rnfn = grnfn
  instance NFData  TH where rnf  = genericRnf

  data TI = I1 (Bool,TH) | I2 | I3 TI TH
    deriving (Show,Generic,Typeable)
  instance NFDataP TI where rnfp = grnfp
  instance NFDataN TI where rnfn = grnfn
  instance NFData  TI where rnf  = genericRnf

  deriveGeneric ''TH
  deriveGeneric ''TI

  expTH_1 = H2 1 [H1 2.3, H3, H4 (H3, I3 I2 (H1 4.5))] False
  expTH_2 = H2 1 [H1 2.3, H3, H4 (__, I3 I2 (H1 4.5))] False
  expTH_3 = H2 1 [H1 2.3, H3, H4 (__, I3 I2 (H1 4.5))] __
  expTH_4 = H2 1 [H1 2.3, H3, H4 (__, I3 __ (H1 4.5))] __

  getH :: TH -> String
  getH (H2 _ [_, _, H4 (_, I3 _ (H1 f))] _) = show f
  getH _ = "getH-!"

#endif

-------------------------------------------------------------------------------

#if USE_SOP

  -- A more elaborate shape for more thorough testing!

  data TJ = J1 Float | J2 (Int,TJ) Bool | J3 | J4 (TJ,TK) | J5 Int | J6 TK
    deriving (Show,Generic,Typeable)
  instance NFDataP TJ where rnfp = grnfp
  instance NFDataN TJ where rnfn = grnfn
  instance NFData  TJ where rnf  = genericRnf

  data TK = K1 (Bool,TJ) | K2 | K3 TK TJ | K4 Int Bool | K5 TJ | K6 Float Int Bool | K7 TJ TJ
    deriving (Show,Generic,Typeable)
  instance NFDataP TK where rnfp = grnfp
  instance NFDataN TK where rnfn = grnfn
  instance NFData  TK where rnf  = genericRnf

  deriveGeneric ''TJ
  deriveGeneric ''TK

  expTJ_1 = J2 (1, J4 (J3, K3 K2 (J1 4.5))) False
  expTJ_2 = J2 (1, J4 (J3, K3 __ (J1 4.5))) False
  expTJ_3 = J2 (1, J4 (__, K3 K2 (J1 4.5))) False
  expTJ_4 = J2 (1, J4 (__, K3 __ (J1 4.5))) __

  getJ :: TJ -> String
  getJ (J2 (_, J4 (_, K3 _ (J1 f))) _) = show f
  getJ _ = "getJ-!"
  getJ' :: TJ -> String

  expTJ_5 = J2 (1, J4 (__, K2)) False

  getJ' ~_ = show 1
--getJ' _ = show 1
--getJ' (J2 ~(n, ~_) _) = show n
--getJ' (J2 (n, _) _) = show n
--getJ' (J2 (n, J4 (_, K2)) _) = show n
--getJ' (J2 (n, J4 (_, K2)) False) = show n  -- most explicit
  getJ' _ = "getJ'-!"

  expTJ_6 = J4 (__, K2)
  getJ6 ~_ = show 1

  expTJ_7 = (__, K2) :: (TJ, TK)
  getJ7 ~_ = show 1

  expTJ_8 = __ :: TJ
  getJ8 ~_ = show 1

  expTK_1 = K3 K2 (J1 4.5)
  expTK_2 = K3 __ (J1 4.5)

  getK :: TK -> String
  getK (K3 _ (J1 f)) = show f
  getK _ = "getK-!"

#endif

-------------------------------------------------------------------------------

#if USE_SOP

  -- Trying to find a minimal failing case now...

#if 1
  data TL = L1 TM Float
#else
  data TL = L1 Float TM
#endif
    deriving (Show,Generic,Typeable)
  instance NFDataP TL where rnfp = grnfp
  instance NFDataN TL where rnfn = grnfn
  instance NFData  TL where rnf  = genericRnf

  data TM = M1 Bool
    deriving (Show,Generic,Typeable)
  instance NFDataP TM where rnfp = grnfp
  instance NFDataN TM where rnfn = grnfn
  instance NFData  TM where rnf  = genericRnf

  deriveGeneric ''TL
  deriveGeneric ''TM

#if 1
  expTL_1 = L1 (M1 True) 5.6
  expTL_2 = L1 (M1 __) 5.6
  expTL_3 = L1 __ 5.6
  expTL_4 = L1 (M1 True) __

  getL :: TL -> String
  getL (L1 (M1 _) f) = show f
  getL _ = "getL-!"
  getL' :: TL -> String
  getL' (L1 (M1 b) _) = show b
  getL' _ = "getL'-!"
#else
  expTL_1 = L1 5.6 (M1 True)
  expTL_2 = L1 5.6 (M1 __)
  expTL_3 = L1 5.6 __
  expTL_4 = L1 __ (M1 True)

  getL :: TL -> String
  getL (L1 f (M1 _)) = show f
  getL _ = "getL-!"
  getL' :: TL -> String
  getL' (L1 _ (M1 b)) = show b
  getL' _ = "getL'-!"
#endif

#endif

-------------------------------------------------------------------------------

