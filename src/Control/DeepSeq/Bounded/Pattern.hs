
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#define DO_TRACE 0

#define USE_POST_ORDER_IDS 0

#define SHOW_PAT_NODE_ATTRS 0

#define WARN_IGNORED_SUBPATTERNS 1
#define NEVER_IGNORE_SUBPATTERNS 0

-- Formerly DEBUG_WITH_DEEPSEQ_GENERICS.
-- Now also needed to force issuance of all compilePat warnings
-- (so not strictly a debugging flag anymore).
-- [Except it didn't work...]
--- #define NFDATA_INSTANCE_PATTERN 0  -- now a .cabal flag

#define DO_DERIVE_DATA_AND_TYPEABLE 0
#define DO_DERIVE_ONLY_TYPEABLE 1
#if DO_DERIVE_ONLY_TYPEABLE && DO_DERIVE_DATA_AND_TYPEABLE
#undef DO_DERIVE_ONLY_TYPEABLE
#warning DO_DERIVE_ONLY_TYPEABLE forced 0, due to DO_DERIVE_DATA_AND_TYPEABLE being 1.
#define DO_DERIVE_ONLY_TYPEABLE 0
#endif

-- Now specified via --flag=[-]USE_WWW_DEEPSEQ
--- #define USE_WW_DEEPSEQ 1

-------------------------------------------------------------------------------

-- Good idea: Let * be followed by an integer N.
-- This shall have the semantics that, when that node
-- is matched in the pattern, instead of rnf it is forcen N'd.

-- There may be fusion possible (which is worth trying here
-- for practise, even if this lib is not used much):
--
--   forcep p1 . forcep p2 = forcep (unionPat [p1,p2])
--
-- This holds if pattern doesn't contain #, or any (type-)constrained
-- subpatterns -- the latter might work out, if exclude # from them too,
-- but I'm not sure.  With #, we lose even monotonicity, let alone
-- the above law.
--
-- For the above to hold, remember, the union must have exactly
-- the "forcing potential" of the LHS -- no more, no less.

-------------------------------------------------------------------------------

#if DO_DERIVE_DATA_AND_TYPEABLE
  {-# LANGUAGE DeriveDataTypeable #-}
#endif
-- XXX Only needed for something in Blah.hs.
-- Check into it, and see if can't get rid of the need
-- for Typeable instances in here!
#if DO_DERIVE_ONLY_TYPEABLE
  {-# LANGUAGE DeriveDataTypeable #-}
#endif

-- Prefer to hand-write the NFData instance, so that can
-- use it with HASKELL98_FRAGMENT.
#if 0
#if NFDATA_INSTANCE_PATTERN
  -- For testing only (controlling trace interleaving):
  {-# LANGUAGE DeriveGeneric #-}
#endif
#endif

  {-  LANGUAGE DeriveFunctor #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Pattern
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Pattern
  (

     -- * Pattern datatype

       Pattern

     , PatNode(..)

     , PatNodeAttrs(..)

     -- * Pattern DSL

     -- | __Grammar__
     --
     -- @
     -- /pat/          /->/  /[/ /modifiers/ /]/ /pat'/
     -- /pat'/         /->/     __.__  /|/  __!__  /|/  __*__ /[/ /decimalint/ /]/  /|/  __(__ /{/ /pat/ /}/ __)__
     -- /modifiers/    /->/  zero or one of each of the eight /modifier/, in any order
     -- /modifier/     /->/     __=__  /|/  __+__  /|/  __^__  /|/  __\/__  /|/  __%__
     --                  /|/  __:__ /typename/ /{/ __;__ /typename/ /}/ __:__
     --                  /|/  __@__ /decimalint/
     --                  /|/  __>__ /permutation/
     -- /typename/     /->/  string containing neither __:__ (unless /escaped/) nor __;__
     -- /escaped/      /->/  __\\\\:__ 
     -- /decimalint/   /->/  digit string not beginning with zero
     -- /permutation/  /->/  of an initial part of the lowercase alphabet, /e.g./ __cdba__
     -- @
     --
     -- Here is the grammar in a more <http://fremissant.net/deepseq-bounded/grammar.html#new-grammar vivid rendering>.
     -- (Haddock makes it tricky to distinguish between metasyntax and concrete syntax.)
     --
     -- Optional whitespace can go between any two tokens (basically,
     -- anyplace there is space shown in the grammar above).
     --
     -- Semicolons never need escaping, because they're already illegal
     -- as part of any Haskell type name.
     --
     -- The semantics are given formally in the 'PatNode' and 'PatNodeAttrs'
     -- documentation, as well as informally in the examples below and from
     -- the project <http://www.fremissant.net/deepseq-bounded homepage>.
     --
     -- __Examples__
     --
     -- @\"__(...)__\"@ will match any ternary constructor.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(!!!)__\" expr@ will force evaluation of @expr@ to a depth of two,
     -- provided the head of @expr@ is a ternary constructor; otherwise it behaves
     -- as @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__.__\" expr@ (/i.e./ do nothing).
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(...)__\" expr@ will force it to only a depth of one. That is,
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(...)__\" expr =
     -- <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp>
     -- \"__!__\" expr@ when the head of @expr@
     -- is a ternary constructor; otherwise it won't perform any evaluation.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__*__\" expr = <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf> expr@.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(***)__\" expr@ will <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf> (deep) any ternary constructor, but
     -- will not touch any constructor of other arity.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(.(*.).)__\" expr@ will match any ternary constructor, then
     -- match the second subexpression constructor if it is binary, and
     -- if matching got this far, then the left sub-subexpression
     -- will be forced (<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf>), but not the right.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(!:T:*.)__\" expr@ will unwrap (shallow 'seq') the first
     -- subexpression of @expr@, and the third subexpression won't be touched.
     -- As for the second subexpression, if its type is @T@ it will be
     -- completely evaluated (<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf>), but otherwise it won't be touched.
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(=**)__\" expr@ will spark the /parallel/ complete evaluation of
     -- the two components of any pair. (Whether the computations actually
     -- run in parallel depends on resource availability, and the discretion
     -- of the RTS, as usual.)
     --
     -- @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataP.html#t:NFDataP rnfp> \"__(>ba(+*+*)=*)__\" expr@ matches a binary constructor, whose first parameter is also a binary constructor. This identifies three main AST branches -- serendipitously symbolised by asterisks -- making up the expression: which branches we'll call __A__, __B__ and __C__. So this example will perform <http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf>, but in a controlled manner: __A__ and __B__ are forced in parallel with __C__, and furthermore, __B__ is forced before __A__. A traceline will be printed at the beginning of the forcing of __B__ and then another traceline will be printed at the beginning of the forcing of __A__. Note that \"__(=>ba(+*+*)*)__\" would be a legal equivalent.
     --
     -- I make no claims as to the usefulness of the examples, they are here just to explain the semantics of the DSL.
     --
     -- __Details__
     --
     -- The present pattern parser ignores any subpatterns of all
     -- pattern nodes except 'WR' and 'TR', optionally emitting a warning.
     -- /(XXX In 0.6.0.0, I'm not sure the warning is still possible.)/
     -- Hence, only 'WR' and 'TR' patterns are potentially recursive.
     --
     -- When specifying the list of subpatterns with 'WR' and 'TR',
     -- in order for the match to succeed, the number of subpatterns must
     -- be equal to the arity of the constructor the pattern node is
     -- matching against. (No other pattern node types accept subpatterns.)
     --
     -- Additionally, in the case of 'TR', matching (and consequent recursion) will only
     -- succeed if the term node has constructor name which is listed in the constraints.
     --
     -- It would be possible to have 'TR' nodes interpret the constraint
     -- as type name rather than constructor name, but this would require /sum patterns/
     -- (maybe for version 0.7).  The problem is, no single 'WR' node
     -- can match multiple constructors of differing arity.  This has the
     -- feel of an excellent application for SOP generics!...
     --
     -- (As contrasted with 'TR' nodes,) 'TI', 'TN' and 'TW' nodes interpret
     -- type constraint strings as type names (not constructor names).
     -- A moment's reflection will show you why it must be so.
     --
     -- Finally, if you're trying to name a constructor with __:__ in its name,
     -- you must escape the colon with a backslash thus __\\\\:__ because
     -- the unescaped colon is used as (opening and) closing character for
     -- lists of type and constructor names.
{--}
     -- Old interjection:
     -- I regret that Haddock cannot offer better markup for distinguishing
     -- the metasyntax.  The bold is not bold enough.  The alternation symbol,
     -- although \/|\/ in the document comment, does not show as slanted for me.
     -- Had no luck using color, also Unicode support seems pretty sketchy.
     -- Embedding an image is possible via data URL, but this has been known
     -- to crash Haddock except for very small images.
{--}
-- I'm still not sure if I try accepting a double-colon close here?
-- (Checking...)
     --              /|/   /(/ __.__  /|/  __*__ /[/ /decimalint/ /]/ /)/ __::__ /typename/ /{/ __;__ /typename/ /}/ __:__ /[/ __:__ /]/

#if 0
-- These are now in Compile (but may move them back once sort out some stuff)
     , compilePat
     , showPat
#endif

     , isWI , isWR , isWS , isWN , isWW , isTI , isTR , isTN , isTW

     , emptyPatNodeAttrs
--   , emptySparkPatNodeAttrs
     , getPatNodeAttrs
     , setPatNodeAttrs
#if USE_PING_PATNODE
     , setPatNodePingParentTID
#endif
     , showPerm
     , showPatRaw
     , showPatNodeRaw

     , setPatternPatNodeUniqueIDs

--   , patternShapeOK  -- useful for defining instances of NFDataP

     -- * Why depend on whole containers package, when we only want a rose tree

     , Rose(..)

     -- * Preferred to have this in Seqable, but had cyclical dependency issues

     , SeqNode(..)

  )
  where

-------------------------------------------------------------------------------

#if DO_DERIVE_DATA_AND_TYPEABLE
  import Data.Data ( Data )
  import Data.Typeable ( Typeable )
#elif DO_DERIVE_ONLY_TYPEABLE
  import Data.Typeable ( Typeable )
#endif

#if USE_WW_DEEPSEQ
  import Control.DeepSeq ( NFData )
#endif

  import Control.Concurrent ( ThreadId )

  import Data.List ( intercalate )
  import Data.Char ( isDigit )
  import Data.Maybe ( isNothing, fromJust )
  import Data.Maybe ( isJust )

  import Debug.Trace ( trace )

#if USE_WW_DEEPSEQ
  -- The only uses of force in this module are for debugging purposes
  -- (including trying to get messages to be displayed in a timely
  -- manner, although that problem has not been completely solved).
  import Control.DeepSeq ( force )
#endif

-- (Hand write this NFData instance for greater portability.)
#if NFDATA_INSTANCE_PATTERN
  -- for helping trace debugging
#if 1
  import Control.DeepSeq
#else
  import qualified Control.DeepSeq.Generics as DSG
  import qualified GHC.Generics as GHC ( Generic )
#endif
#endif

  import Data.Char ( ord )
  import Data.Char ( chr )

  import Control.Monad.State as ST

-- Can't do it here, due to cyclical imports. (Surely this
-- could be handled better by the tools...).
#if 0
#if OVERLOADED_STRINGS
  import GHC.Exts( IsString(..) )
  import Control.DeepSeq.Bounded.Compile ( compilePat )
#endif
#endif

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
#else
  mytrace _ = id
#endif

-------------------------------------------------------------------------------

  data Rose a = Node a [ Rose a ]
-- (Hand write this NFData instance for greater portability.)
#if 0 && NFDATA_INSTANCE_PATTERN
#if DO_DERIVE_DATA_AND_TYPEABLE
   deriving (Show, Eq, GHC.Generic, Data, Typeable)
-- deriving (Show, Eq, Functor, GHC.Generic, Data, Typeable)
#elif DO_DERIVE_ONLY_TYPEABLE
   deriving (Show, Eq, GHC.Generic, Typeable)
#else
   deriving (Show, Eq, GHC.Generic)
#endif
#else
#if DO_DERIVE_DATA_AND_TYPEABLE
   deriving (Show, Eq, Data, Typeable)
#elif DO_DERIVE_ONLY_TYPEABLE
   deriving (Show, Eq, Typeable)
#else
   deriving (Show, Eq)
#endif
#endif
  type Pattern = Rose PatNode

  instance NFData a => NFData (Rose a) where
    rnf (Node x chs) = rnf x `seq` rnf chs

  instance Functor Rose where
    fmap f (Node x chs) = Node (f x) (map (fmap f) chs)

-- (Hand write this NFData instance for greater portability.)
#if 0
#if NFDATA_INSTANCE_PATTERN
  instance NFData a => NFData (Rose a) where rnf = DSG.genericRnf
#endif
#endif

-- (See note at import of IsString; this instance is in Compile.hs.)
#if 0
#if OVERLOADED_STRINGS
  instance IsString (Rose PatNode) where
--instance IsString Pattern where
    fromString = compilePat  -- aahhhh..... (20150204)
#endif
#endif

-------------------------------------------------------------------------------

-- XXX
--
-- A major design decision needs to be made [does it?...]:
-- Is PatNode to remain a SUM ( ctor1 | ctor2 | ... ) or should
-- it be refactored to be a PRODUCT, i.e. a new field called
-- patNodeKind :: PatNodeKind, and all the fields of PatNodeAttrs
-- are lifted up to be siblings of patNodeKind in a single
-- top-level product (and PatNodeAttrs identifier elided).
--
-- The SUM has the advantage of convenient pattern-matching,
-- whereas the PRODUCT ... well, since we're using record syntax
-- (not yet in PatNode though!...), we CAN (I just discovered!)
-- do plain H98 pattern matching on multi-parameter constructors,
-- projecting out JUST any one value into a fresh pattern variable.
--
-- So in light of that, the refactoring to product has a lot to
-- recommend it -- however, will there not be a runtime penalty
-- of an extra wrapper or selector application or something?...
--
-- Am I missing anything crucial?...
--
-- If PatNode became a newtype, we pay only a compile-time price, right?...
--
-- Reading:
--
--   newtype Age = Age { unAge :: Int }
-- brings into scope both a constructor and a de-constructor:
--      Age :: Int -> Age
--    unAge :: Age -> Int
--
-- Thanks! I was just trying to remember this.
-- So incidentally, newtypes allow record syntax but only for
-- SINGLE PARAMETER constructor. (So newtypes will be of little
-- use if we adopted the product design above.)

-------------------------------------------------------------------------------

-- XXX be truly banished from the code via build flags (USE_PAR_PATNODE etc.).
--  - if use cpp (not cpphs) to preprocess, that will appear as 0 or 1 in
--    the documentation...

  -- | These attributes can be mixed freely. Certain combinations may
  -- seem unuseful, but nothing is prohibited by design.
  --
  -- While this may seem bloated, most of these capabilities can
  -- be truly banished from the code via build flags (__use_par_patnode__, etc.).
  --
  -- In the concrete pattern syntax, all attributes are represented
  -- as prefix modifiers (prefixing the
  -- @\'.\'@, @\'!\'@, @\'(\'@ or @\'*\'@
  -- pattern node designator).
  -- Prefix modifiers may be given in any order.
  --
  -- / NOTE: The 'depth' field in 'PatNodeAttrs' is not really an attribute, /
  -- / it is logically a (mandatory) extra parameter to 'WN' and 'TN' /
  -- / nodes (and only those).  (Whereas attributes are all optional /
  -- / and node-type-agnostic.)  The 'depth' is stored here as a /
  -- / convenient hack only!  Explanation becomes necessary since /
  -- / Haddock makes it visible no matter what, I mention this, in case /
  -- / of confusion, because the 'depth' is always __post__fix, not prefix. /
  -- 
{--}
  -- XXX Is there any particular reason these fields should be marked strict?
  -- Should they be explicitly unboxed as well? (Performance has been adequate
  -- for the purposes so far...).
  data PatNodeAttrs
         = PatNodeAttrs {
               uniqueID :: !Int              -- ^ Optional for convenience; set up with 'setPatternPatNodeUniqueIDs'. Beware that this function is not called automatically (or if it happens to be at the moment, this behaviour shouldn't be relied upon). For example, if you were to call <http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-PatUtil.html#v:growPat growPat>, the added nodes would all have \"uniqueID\" of 0.
             , depth :: !Int                 -- ^ (__*__/n/) &#8195; Depth of forcing for 'WN' and 'TN' nodes (/n/ is decimal integer depth). /(This is not an attribute, it's a displaced mandatory parameter, specific to these two node types.)/
             , doConstrainType :: !Bool      -- ^ (__:__) &#8195; Constrain pattern to match only types named in 'typeConstraints'. /__XXX__ This should be considered experimental still in 0.6. This and the "NFDataPDyn" aspects lost attention to/ <http://hackage.haskell.org/package/seqaid seqaid>.
             , typeConstraints :: ![String]  -- ^ The list of type rep strings used in the type constraint (when 'doConstrainType' is 'True').
             , doDelay :: !Bool              -- ^ (__@__) &#8195; Delay (current thread) for 'delayus' microseconds. /__XXX__ Still buggy?/
             , delayus :: !Int               -- ^ Microseconds of delay (when 'doDelay' is 'True').
#if USE_PAR_PATNODE
             , doSpark :: !Bool              -- ^ (__=__) &#8195; Spark matching for parallel evaluation.
#endif
#if USE_PSEQ_PATNODE
             , doPseq :: !Bool               -- ^ (__>__/perm/) &#8195; Sequence child subpattern matching, according to the permutation in 'pseqPerm'.
             , pseqPerm :: Maybe [Int]       -- ^ Lowercase alphabetic sequence is used in the concrete pattern syntax.  @__>cdba(wxyz)__@ will cause subpattern matching recursion on a quaternary constructor, with the subpattern computations sequenced @__y__@ then @__z__@ then @__x__@ then @__w__@ (order corresponds to @__cdba__@).
{-[XXX Until it really is, better not leave it saying so!] It is a runtime error (with a message), tested during matching, if this is a Just value and the list is not compatibly sized with the subpatterns. Sequencing syntax therefore would only work with 'WR' and 'TR' nodes, so we trap for the other cases and give a suitable error message.-}
#endif
#if USE_TRACE_PATNODE
             , doTrace :: !Bool              -- ^ (__+__) &#8195; Output a traceline to stderr.
#endif
#if USE_PING_PATNODE
             , doPing :: !Bool               -- ^ (__^__) &#8195; Raise informative (asynchronous? support is not strong for it, <http://hackage.haskell.org/package/base/docs/Control-Exception.html#v:throwTo throwTo> blocks...) exception /en passant/, for benefit of upstream. The exception is thrown in a new thread, so that the pattern matching continues; for a terminating version, see 'doDie'.
             , pingParentTID :: Maybe ThreadId  -- ^ Needed as argument for 'throwTo' call.
#endif
#if USE_DIE_PATNODE
             , doDie :: !Bool                -- ^ (__/__) &#8195; Kill (just this) thread.
#endif
#if USE_TIMING_PATNODE
             , doTiming           :: !Bool   -- ^ (__%__) &#8195; Note time passed since pattern-matched parent node. /__XXX__ Work in progress./
             , timestamp          :: !Int  -- XXX for now
             , parent_timestamp   :: !Int
             , delta_timestamp    :: !Int
#endif
           }
-- (Hand write this NFData instance for greater portability.)
#if 0 && NFDATA_INSTANCE_PATTERN
#if DO_DERIVE_DATA_AND_TYPEABLE
       deriving ( Show, Eq, Typeable, Data, GHC.Generic )
#elif DO_DERIVE_ONLY_TYPEABLE
       deriving ( Show, Eq, Typeable, GHC.Generic )
#else
       deriving ( Show, Eq, GHC.Generic )
#endif
#else
#if DO_DERIVE_DATA_AND_TYPEABLE
       deriving ( Show, Eq, Typeable )  -- Data apparently not needed
#elif DO_DERIVE_ONLY_TYPEABLE
       deriving ( Show, Eq, Typeable )
#else
       deriving ( Show, Eq )
#endif
#endif

#if NFDATA_INSTANCE_PATTERN
  instance NFData ThreadId where rnf x = ()
-- (Hand write this NFData instance for greater portability.)
--instance NFData PatNodeAttrs where rnf = DSG.genericRnf
  instance NFData PatNodeAttrs where
    rnf (PatNodeAttrs
           uniqueID
           depth
           doConstrainType
           typeConstraints
           doDelay
           delayus
#if USE_PAR_PATNODE
           doSpark
#endif
#if USE_PSEQ_PATNODE
           doPseq
           pseqPerm
#endif
#if USE_TRACE_PATNODE
           doTrace
#endif
#if USE_PING_PATNODE
           doPing
           pingParentTID
#endif
#if USE_DIE_PATNODE
           doDie
#endif
#if USE_TIMING_PATNODE
           doTiming
           timestamp
           parent_timestamp
           delta_timestamp
#endif
        )
     =
             uniqueID
       `seq` rnf depth
       `seq` rnf doConstrainType
       `seq` rnf typeConstraints
       `seq` rnf doDelay
       `seq` rnf delayus
#if USE_PAR_PATNODE
       `seq` rnf doSpark
#endif
#if USE_PSEQ_PATNODE
       `seq` rnf doPseq
       `seq` rnf pseqPerm
#endif
#if USE_TRACE_PATNODE
       `seq` rnf doTrace
#endif
#if USE_PING_PATNODE
       `seq` rnf doPing
       `seq` rnf pingParentTID
#endif
#if USE_DIE_PATNODE
       `seq` rnf doDie
#endif
#if USE_TIMING_PATNODE
       `seq` rnf doTiming
       `seq` rnf timestamp
       `seq` rnf parent_timestamp
       `seq` rnf delta_timestamp
#endif
#endif

  emptyPatNodeAttrs :: PatNodeAttrs
  emptyPatNodeAttrs
   = PatNodeAttrs {
         uniqueID           = 0
       , depth              = 0
       , doConstrainType    = False
       , typeConstraints    = []
       , doDelay            = False
       , delayus            = 0
#if USE_PAR_PATNODE
       , doSpark            = False
#endif
#if USE_PSEQ_PATNODE
       , doPseq             = False
       , pseqPerm           = Nothing
#endif
#if USE_TRACE_PATNODE
       , doTrace            = False
#endif
#if USE_PING_PATNODE
       , doPing             = False
       , pingParentTID      = Nothing
#endif
#if USE_DIE_PATNODE
       , doDie              = False
#endif
#if USE_TIMING_PATNODE
       , doTiming           = False
       , timestamp          = 0  -- Int for now
       , parent_timestamp   = 0
       , delta_timestamp    = 0
#endif
     }

-- (later: seems unused so commenting out)
--emptySparkPatNodeAttrs :: PatNodeAttrs
--emptySparkPatNodeAttrs = emptyPatNodeAttrs { doSpark = True }

  getPatNodeAttrs :: PatNode -> PatNodeAttrs
  getPatNodeAttrs pas = case pas of
    WI as -> as
    WS as -> as
    WR as -> as
    WN as -> as
#if USE_WW_DEEPSEQ
    WW as -> as
#endif
    TI as -> as
--  TS as -> as
    TR as -> as
    TN as -> as
#if USE_WW_DEEPSEQ
    TW as -> as
#endif
    _ -> error $ "getPatNodeAttrs: unexpected PatNode: " ++ show pas

  setPatNodeAttrs :: PatNode -> PatNodeAttrs -> PatNode
  setPatNodeAttrs pas as' = case pas of
    WI _ -> WI as'
    WS _ -> WS as'
    WR _ -> WR as'
    WN _ -> WN as'
#if USE_WW_DEEPSEQ
    WW _ -> WW as'
#endif
    TI _ -> TI as'
--  TS _ -> TS as'
    TR _ -> TR as'
    TN _ -> TN as'
#if USE_WW_DEEPSEQ
    TW _ -> TW as'
#endif
    _ -> error $ "setPatNodeAttrs: unexpected PatNode: " ++ show pas

#if USE_PING_PATNODE
  setPatNodePingParentTID :: ThreadId -> PatNode -> PatNode
  setPatNodePingParentTID tid pn = pn'
   where pn' = let as' = (getPatNodeAttrs pn) { pingParentTID = Just tid }
               in setPatNodeAttrs pn as'
#endif

  showPerm :: Maybe [Int] -> String
  showPerm Nothing = ""
  showPerm (Just lst) = showPerm' lst
  showPerm' [] = ""
  showPerm' (i:is) = (chr (i + ord 'a')) : showPerm' is

  -- Refer to http://stackoverflow.com/questions/12658443/how-to-decorate-a-tree-in-haskell/12658639 (among other SO questions) for good info and options.
  -- I've opted to remain H98 here, folloiwng Luis Casillas' answer.
  setPatternPatNodeUniqueIDs :: Int -> Pattern -> Pattern
  setPatternPatNodeUniqueIDs n pat
#if USE_POST_ORDER_IDS
   = ST.evalState (mapRoseM step pat) n
#else
   = ST.evalState (mapRoseM' step pat) n
#endif
   where
#if 1
    step :: PatNode -> ST.State Int PatNode
    step pn = do tag <- postIncrement
                 let as = getPatNodeAttrs pn
                 let as' = as { uniqueID = tag }
                 let pn' = setPatNodeAttrs pn as'
                 return pn'
#else
    step :: Pattern -> ST.State Int Pattern
    step p = do tag <- postIncrement
                let Node pn cs = p
                let p' = Node (pn { uniqueID = tag }) cs
                return p'
--              return (p, tag)
#endif

#if 1

-- This is from Luis Casillas' answer.

#if 0
  -- This function is not part of the solution, but it will help you
  -- understand mapRoseM below.
  mapRose :: (a -> b) -> Rose a -> Rose b
  mapRose fn (Node a subtrees) =
      let subtrees' = map (mapRose fn) subtrees
          a' = fn a
       in Node a' subtrees'

  -- Normally you'd write that function like this:
  mapRose' fn (Node a subtrees) = Node (fn a) $ map (mapRose' fn) subtrees
#endif

  -- But I wrote it out the long way to bring out the similarity to the
  -- following, which extracts the structure of the tagStep definition from
  -- the first solution above.
  mapRoseM :: Monad m => (a -> m b) -> Rose a -> m (Rose b)
  mapRoseM action (Node a subtrees) =
      do subtrees' <- mapM (mapRoseM action) subtrees
         a' <- action a
         return $ Node a' subtrees'

  -- That whole business with getting the state and putting the successor
  -- in as the replacement can be abstracted out.  This action is like a
  -- post-increment operator.
  postIncrement :: Enum s => ST.State s s
  postIncrement = do val <- ST.get
                     ST.put (succ val)
                     return val

#if 0
  -- Now tag can be easily written in terms of those.
  tag init tree = evalState (mapRoseM step tree) init
      where step a = do tag <- postIncrement
                        return (a, tag)
#endif

  -- You can make mapRoseM process the local value
  -- before the subtrees if you want:
  mapRoseM' action (Node a subtrees) =
      do a' <- action a
         subtrees' <- mapM (mapRoseM' action) subtrees
         return $ Node a' subtrees'

#if 0
  -- And using Control.Monad you can turn this into a one-liner:
  mapRoseM action (Node a subtrees) =
      -- Apply the Rose constructor to the results of the two actions
      liftM2 Node (action a) (mapM (mapRoseM action) subtrees)

  -- Or in children-first order:
  mapRoseM' action (Node a subtrees) =
      liftM2 (flip Node ) (mapM (mapRoseM action) subtrees) (action a)
#endif

#endif

-------------------------------------------------------------------------------

  -- XXX Is there any particular reason these fields should be marked strict?

  -- | Only 'WR' and 'TR' allow for explicit recursion.
  --
  -- All other 'PatNode' values are in leaf position when they occur.
  --
  -- Concrete syntax for @W*@ and @T*@ nodes are identical. A @T*@ node
  -- is simply a @W*@ node bearing a type constraint attribute.
  -- Please refer to the __Grammar__ further down this page for more details,
  -- and links to even more information.
  --
  -- /Notes:/
  --
  -- /I've kept the @T*@ types broken out as separate constructors, although they could be handled as special cases of @W*@ types in a way analogous to 'doSpark' ('PatNodeAttrs').  These were not \"absorbed\" because the semantics seems icky, and it's still not clear which @W*@ types even make sense with a type constraint.../
  --
  -- /I tried parametrising this, but it messed up my Show instance and seemed to be pulling me away from Haskell 98, so reverted.  It looks a bit ugly in the Haddock unfortunately, with the redundant column of @PatNodeAttrs@. The @T*@ nodes will be absorbed by 'PatNodeAttrs' in version 0.7, and it won't look so bad./
  data PatNode
       =

-- PatNodeAttrs was strict, but changed it b/c had problems using TN{}
-- form in some places (though not, apparantly, in all places)...
         WI !PatNodeAttrs  -- ^ (/__I__nsulate/, __.__ ) &#8195; Don't even unwrap the constructor of this node.
       | WR !PatNodeAttrs  -- ^ (/__R__ecurse/, __(__...__)__ ) &#8195; Continue pattern matching descendants, provided that arity is compatible (else the match fails).  Interior nodes of a pattern are always @WR@, /i.e./ @WR@ is the only @PatNode@ offering explicit recursion.  The rest (@?S@, @?N@, and @?W@) are implicitly recursive, but control is only as powerful as "NFDataN".
       | WS !PatNodeAttrs  -- ^ (/__S__top/, __!__ ) &#8195; Stop recursing (nothing more forced down this branch). This is equivalent to 'WN' at a 'depth' of 1. /'WS' is somewhat vestigial, and may be removed in 0.7./
       | WN !PatNodeAttrs  -- ^ (/__N__ (depth)/, __*__n ) &#8195; @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataN.html#t:NFDataN rnfn> n@ the branch under this node.
#if USE_WW_DEEPSEQ
       | WW !PatNodeAttrs  -- ^ (/__W__/ild, __*__ ) &#8195; Fully force (<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf>) the whole branch under this node. /Note that this is/ not /achievable as a limiting case of 'WN', so the existence of 'WW' is formally justifiable in a way that 'WS' is not. Having said that, for all practical purposes, a 'WN' with 'depth' @= maxBound::Int@ could be used for 'WW'.../
#endif
{--} -- XXX It's still unclear whether TI should allow subpatterns;
-- the alternative is for TI, when type doesn't match, to behave
-- as "." (no subpatterns); but since I say "otherwise behave as TR",
-- and TR says "continue pattern matching descendants", this seems to
-- say that subpatterns should be permitted.  Certainly it's no problem
-- to permit subpatterns in this case, but WI should still ignore
-- subpatterns since it will always be # regardless of node type.
-- (Subpatterns ought to be "safely redundant" in this case, but whether
-- they are depends on implementation and needs to be tested if allow
-- WI subpatterns to survive past the parser/compiler!)
--   And this all applies to TW and TN too, right? Yes.
-- It seems clear that TI, TW and TN should all allow subpatterns.
-- And that WI, WW and WN should elide them and issue a warning.
--   But, none of my present woes seem to be connected with this...
-- Nonetheless, it's important to pin down the semantics.
-- XXX Jan. '15: Soon these T* nodes will disappear, and the corresponding
-- PatNodeAttrs attributes will be used, as did for the P* (now doSpark) nodes.
       | TI !PatNodeAttrs  -- ^ Don't even unwrap the constructor of this node, if it's type is in the list; otherwise behave as 'WW'. (Note this behaviour is the complement of 'TW' behaviour.)
       | TR !PatNodeAttrs  -- ^ Match any of the types in the list (and continue pattern matching descendants); behave as 'WI' for nodes of type not in the list.
---    | TS !PatNodeAttrs  -- (never existed)
       | TN !PatNodeAttrs  -- ^ @<http://hackage.haskell.org/package/deepseq-bounded-0.8.0.0/docs/Control-DeepSeq-Bounded-NFDataN.html#t:NFDataN rnfn> n@ the branch under this node, if the node type matches any of the types in the list; otherwise behave as 'WI'.
#if USE_WW_DEEPSEQ
       | TW !PatNodeAttrs  -- ^ Fully force (<http://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData rnf>) the whole branch under this node, if the node type matches any of the types in the list; otherwise behave as 'WI'. (Note this behaviour is the complement of 'TI' behaviour.)
#endif
       | XX                -- ^ Dummy node type reserved for internal use.

-- (Hand write this NFData instance for greater portability.)
#if 0 && NFDATA_INSTANCE_PATTERN
#if DO_DERIVE_DATA_AND_TYPEABLE
       deriving ( Eq, Typeable, Data, GHC.Generic )
#elif DO_DERIVE_ONLY_TYPEABLE
       deriving ( Eq, Typeable, GHC.Generic )
#else
       deriving ( Eq, GHC.Generic )
#endif
#else
#if DO_DERIVE_DATA_AND_TYPEABLE
       deriving ( Eq, Typeable )  -- Data apparently not needed
#elif DO_DERIVE_ONLY_TYPEABLE
       deriving ( Eq, Typeable )
#else
       deriving ( Eq )
#endif
#endif

#if NFDATA_INSTANCE_PATTERN
-- (Hand write this NFData instance for greater portability.)
--instance NFData PatNode where rnf = DSG.genericRnf
  instance NFData PatNode where
    rnf pas = rnf as where as = getPatNodeAttrs pas
#if 0
    rnf WI = True ; isWI _ = False
    rnf WR = True ; isWR _ = False
    rnf WS = True ; isWS _ = False
    rnf WN = True ; isWN _ = False
#if USE_WW_DEEPSEQ
    rnf WW = True ; isWW _ = False
#endif
    rnf TI = True ; isTI _ = False
    rnf TR = True ; isTR _ = False
    rnf TN as = True ; isTN _ = False
#if USE_WW_DEEPSEQ
    rnf TW as = True ; isTW _ = False
#endif
#endif
#endif

  isWI WI{} = True ; isWI _ = False
  isWR WR{} = True ; isWR _ = False
  isWS WS{} = True ; isWS _ = False
  isWN WN{} = True ; isWN _ = False
#if USE_WW_DEEPSEQ
  isWW WW{} = True ; isWW _ = False
#endif
  isTI TI{} = True ; isTI _ = False
  isTR TR{} = True ; isTR _ = False
  isTN TN{} = True ; isTN _ = False
#if USE_WW_DEEPSEQ
  isTW TW{} = True ; isTW _ = False
#endif

  instance Show PatNode where
#if SHOW_PAT_NODE_ATTRS

    show (WI as) = "WI " ++ show as
    show (WR as) = "WR " ++ show as
    show (WS as) = "WS " ++ show as
    show (WN as) = "WN " ++ show as
#if USE_WW_DEEPSEQ
    show (WW as) = "WW " ++ show as
#endif
    show (TI as) = "TI " ++ show as
    show (TR as) = "TR " ++ show as
    show (TN as) = "TN " ++ show as
#if USE_WW_DEEPSEQ
    show (TW as) = "TW " ++ show as
#endif

#else

    show pas
     | WI{} <- pas = s1++"WI"++s2
     | WR{} <- pas = s1++"WR"++s2
     | WS{} <- pas = s1++"WS"++s2
     | WN{} <- pas = s1++"WN"++s2'
#if USE_WW_DEEPSEQ
     | WW{} <- pas = s1++"WW"++s2
#endif
     | TI{} <- pas = s1++"TI"++s2''
     | TR{} <- pas = s1++"TR"++s2''
     | TN{} <- pas = s1++"TN"++s2'''
#if USE_WW_DEEPSEQ
     | TW{} <- pas = s1++"TW"++s2''
#endif
     where
      as = getPatNodeAttrs pas
      s1 =    ""
           ++ (if doDelay as then "@" ++ (show $ delayus as) else "")
#if USE_PAR_PATNODE
           ++ (if doSpark as then "=" else "")
#endif
#if USE_PSEQ_PATNODE
           ++ (if doPseq  as then ">" ++ (showPerm $ pseqPerm as) else "")
#endif
#if USE_TRACE_PATNODE
           ++ (if doTrace as then "+" else "")
#endif
#if USE_PING_PATNODE
           ++ (if doPing  as then "^" else "")
#endif
#if USE_DIE_PATNODE
           ++ (if doDie   as then "/" else "")
#endif
#if USE_TIMING_PATNODE
           ++ (if doTiming as then "%" else "")
#endif
      s2 = ""
      s2' = " " ++ show (depth as)
      s2'' = " (" ++ intercalate ";" (typeConstraints as) ++ ")"
      s2''' = s2' ++ s2''
#if 0
      doubleBackslashes :: String -> String
      doubleBackslashes ('\\':t) = '\\':'\\':doubleBackslashes t
      doubleBackslashes (h:t) = h:doubleBackslashes t
      doubleBackslashes [] = []
#endif

#endif

  showPatRaw :: Pattern -> String
  showPatRaw (Node pn cs) = showPatNodeRaw pn ++ "\n[" ++ intercalate "," (map showPatRaw cs) ++ "]"
  showPatNodeRaw :: PatNode -> String
  showPatNodeRaw (WI as) = "WI "++show as
  showPatNodeRaw (WR as) = "WR "++show as
  showPatNodeRaw (WS as) = "WS "++show as
  showPatNodeRaw (WN as) = "WN "++show as
#if USE_WW_DEEPSEQ
  showPatNodeRaw (WW as) = "WW "++show as
#endif
  showPatNodeRaw (TI as) = "TI "++show as
  showPatNodeRaw (TR as) = "TR "++show as
  showPatNodeRaw (TN as) = "TN "++show as
#if USE_WW_DEEPSEQ
  showPatNodeRaw (TW as) = "TW "++show as
#endif

-------------------------------------------------------------------------------

#if 0
  patternShapeOK :: Data a => Pattern -> a -> Bool
  patternShapeOK pat x = S.shapeOf pat == S.shapeOf x
#endif

-------------------------------------------------------------------------------

  -- Note that Ord is derived, so the order that the constructors
  -- are listed matters!  (This only affects GHC rules, SFAIK.)
  -- (This data type is here, to avoid cyclical imports which
  -- GHC pretty much is useless with.)
  --------
  -- On the one hand, we want to keep this lightweight -- it can in
  -- principle be a single bit (Insulate/Propagate), as originally planned!
  -- But the Spark thing was too useful; and Print and Error would
  -- also be useful.  But they're more orthogonal.
  --------
  -- Later: Went with PatNodeAttrs for Pattern, but for Seqable
  -- we really prefer to keep it swift and simple for a while yet.
#if 0
  type Spark = Bool
  type PrintPeriod = Int
  type ErrorMsg = String
  data SeqNode =
           Insulate Spark PrintPeriod
         | Conduct Spark PrintPeriod
         | Force Spark PrintPeriod
         | Error ErrorMsg
    deriving ( Eq, Ord )
#else
  -- Later: Maybe "Insulate" is too strong a word. If going to "Insulate",
  -- then "external demand" (like demand generated by natural program
  -- evaluation) should also be repulsed (probably throwing an Error).
  -- "Conduct" would then permit these external demands to propagate,
  -- but will not cause any additional forcing.
  -- Then "Propagate" (which we can call "Force" now?) would carry
  -- the artificial forcing demands.
  -- So yeah, pretty much the above scheme, but try to get it happening
  -- for natural demand.
  -- If we can print trace info for every node, we can trace natural demand.
  -- And use that info, dynamically, to configure the harness.  In a sense
  -- it seems silly to do that, since that's what the RTS is already doing,
  -- but it's an important special case. If the natural demand pattern
  -- is constant (over a window), then (within that window) we can
  -- safely manipulate the harness so long as its artificial forcing
  -- doesn't extend beyond those natural bounds.  Why would you want
  -- to do this? I'm not sure, but it has theoretical interest at least.
  -- Because, for one thing, you are no longer at risk of changing
  -- the semantics by introducing new bottoms.  And so if the demand
  -- pattern is BIG, we could use the seqharn to parallelise it etc?
  -- Just the natural demand that is...  I hope so. That's the ticket.
  data SeqNode =
           Insulate
---      | Conduct
         | Propagate  -- XXX if include Conduct, then rename Propagate to Force
#if USE_PAR_SEQABLE
         | Spark
#endif
-- These would break the Ord; and besides, they're sort of orthogonal
-- (as is Spark)
---      | Print Int
---      | Error String
    deriving ( Eq, Ord )
--  deriving ( Eq, Ord, Show )
#endif

-------------------------------------------------------------------------------

