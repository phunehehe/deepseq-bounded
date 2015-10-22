
-------------------------------------------------------------------------------

  {-  LANGUAGE CPP #-}

#if OVERLOADED_STRINGS
{-# LANGUAGE FlexibleInstances #-}
{-  LANGUAGE TypeSynonymInstances #-}
#endif

#define DO_TRACE 0

#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_NUMBER_ALONE__SAFE_ONLY_TO_DEPTH_19 && ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
#error Please set at most one of the flags ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_NUMBER_ALONE__SAFE_ONLY_TO_DEPTH_19 and ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9 to True.
#endif


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

#if DO_DERIVE_DATA_AND_TYPEABLE
  {-# LANGUAGE DeriveDataTypeable #-}
#endif
-- XXX Only needed for something in Blah.hs.
-- Check into it, and see if can't get rid of the need
-- for Typeable instances in here!
#if DO_DERIVE_ONLY_TYPEABLE
  {-# LANGUAGE DeriveDataTypeable #-}
#endif
#if NFDATA_INSTANCE_PATTERN
  -- For testing only (controlling trace interleaving):
  {-# LANGUAGE DeriveGeneric #-}
#endif
  {-  LANGUAGE DeriveFunctor #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq.Bounded.Compile
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable (H98)
--

-------------------------------------------------------------------------------

  module Control.DeepSeq.Bounded.Compile
  (

      compilePat
    , showPat

---   compilePat'  ,

  )
  where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded.Pattern
  import Control.DeepSeq.Bounded.PatUtil ( liftPats )

  import Data.Char ( isSpace )
  import Data.Char ( isLower )
  import Data.Char ( ord )
  import Data.Char ( isDigit )
  import Data.List ( intercalate )
  import Data.List ( sort )
  import Data.Maybe ( isNothing, fromJust )
  import Data.Maybe ( isJust )

#if 0

#if DO_DERIVE_DATA_AND_TYPEABLE
  import Data.Data ( Data )
  import Data.Typeable ( Typeable )
#elif DO_DERIVE_ONLY_TYPEABLE
  import Data.Typeable ( Typeable )
#endif

#if USE_WW_DEEPSEQ
  import Control.DeepSeq ( NFData )
#endif

#endif

  import Debug.Trace ( trace )

  -- The only uses of force in this module are for debugging purposes
  -- (including trying to get messages to be displayed in a timely
  -- manner, although that problem has not been completely solved).
  import Control.DeepSeq ( force )
#if 0
#if NFDATA_INSTANCE_PATTERN
  -- for helping trace debugging
  import qualified Control.DeepSeq.Generics as DSG
  import qualified GHC.Generics as GHC ( Generic )
#endif
#endif

-- Want it in Pattern with other Pattern instances, but it
-- makes a cyclical dependency and we all know that is more
-- trouble than it's worth in GHC (and other Haskell compilers
-- I've known)...
#if OVERLOADED_STRINGS
  import GHC.Exts( IsString(..) )
#endif

-------------------------------------------------------------------------------

-- (See note at import of IsString; this instance should be in Pattern.hs.)
#if OVERLOADED_STRINGS
--instance IsString (Rose PatNode) where
  instance IsString Pattern where
    fromString = compilePat  -- aahhhh..... (20150204)
#endif

-------------------------------------------------------------------------------

#if DO_TRACE
  mytrace = trace
#else
  mytrace _ = id
#endif

  -- XXX This is still lacks support for the two condensed grammars:
  -- ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_NUMBER_ALONE__SAFE_ONLY_TO_DEPTH_19
  -- ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9

  compilePat' :: String -> Pattern
  compilePat' s
   | not $ null s'  = error $ "\ncompilePat: input rejected: "
                       ++ s
                       ++ if isNothing mmsg then "" else "\nParser message: "
                       ++ fromJust mmsg
                       ++ "\nPatterns parsed so far: ["
                       ++ intercalate ", " (map show pats)
                       ++ "]"
   | otherwise      = case pats of
       [] -> error $ "\ncompilePat: "
              ++ if null s then "empty input" else "vacuous input"
       [pat] -> setPatternPatNodeUniqueIDs 0 pat
       pats -> setPatternPatNodeUniqueIDs 0 $ liftPats pats
   where (pats, mmsg, s') = compilePats s []

  -- String in last component of result is unconsumed input.

  compilePats :: String -> [Pattern] -> ([Pattern], Maybe String, String)
  compilePats s acc
   | null s_ltrim  = (reverse acc, Nothing, s_ltrim)
   | otherwise     = case cpat s of
      (Left "", s') -> (reverse acc, Nothing, s')
--    (Left "", s') -> compilePats s' acc
      (Left msg, s') -> (reverse acc, Just msg, s')
      (Right pat, s') -> compilePats s' (pat:acc)
   where s_ltrim = dropWhile isSpace s

-- XXX Don't forget to do a post-pass to change W* nodes
-- to corresponding T* nodes, when : modifier was present!
-- Oops, guess I did it here; but the original idea would
-- be less cloning...

  cpat :: String -> (Either String Pattern, String)
--cpat _ | trace "J-1: " $ False  = undefined
  cpat [] = (Left "unexpected end of input", [])
--cpat s | trace ("J-2: "++show s) $ False  = undefined
  cpat s
   | null s''   = error "\ncompilePat: type constraint must precede a pattern node"
   | isW        = case c of
      '.' -> (Right $ Node (WI as) [], cs)
      '!' -> (Right $ Node (WS as) [], cs)
      '*' -> case parseInt cs [] of
               (Nothing, cs'') -> (Right $ Node (WW   as    ) [], cs'')
               (Just is, cs'') -> (Right $ Node (WN $ asn is) [], cs'')
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
      '{' ->
#else
      '(' ->
#endif
             if isNothing mmsg_subpats
             then (Right $ Node (WR as) subpats, cs_subpats)
             else (Left $ fromJust mmsg_subpats, cs_subpats)
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
      '}' ->
#else
      ')' ->
#endif
             (Left "", cs)
      c -> error $ "\ncompilePat: unexpected character '" ++ [c] ++ "'"
   | otherwise  = case c of
      '.' -> (Right $ Node (TI as) [], cs)
--    '!' -> (Right $ Node (TS as) [], cs)
      '*' -> case parseInt cs [] of
               (Nothing, cs'') -> (Right $ Node (TW   as    ) [], cs'')
               (Just is, cs'') -> (Right $ Node (TN $ asn is) [], cs'')
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
      '{' ->
#else
      '(' ->
#endif
             if isNothing mmsg_subpats
             then (Right $ Node (TR as) subpats, cs_subpats)
             else (Left $ fromJust mmsg_subpats, cs_subpats)
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
      '}' ->
#else
      ')' ->
#endif
             (Left "", cs)
      c -> error $ "\ncompilePat: unexpected character '" ++ [c] ++ "'"
   where
    s' = dropWhile isSpace s
    (c:cs) = s''
    (as_mods, mmsg_mods, s'') = cmods s'  -- collect any prefix modifiers
    as = case mmsg_mods of
           Nothing -> as_mods
           Just msg -> error $ "\ncompilePat: " ++ msg
    asn is = as { depth = read is :: Int }
    isW = not $ doConstrainType as
    (subpats, mmsg_subpats, cs_subpats) = compilePats cs []

  -- Accumulate any prefix modifiers into an empty PatNodeAttrs structure.
  cmods :: String -> (PatNodeAttrs, Maybe String, String)
  cmods s = cmods' s emptyPatNodeAttrs
  cmods' :: String -> PatNodeAttrs -> (PatNodeAttrs, Maybe String, String)
  cmods' [] as = (as, Nothing, [])
--cmods' [] as = (as, Just "cmods': unexpected end of input", [])
  cmods' s as = case c of
    ':' -> cmods' cs_types  as_types
    '@' -> cmods' cs_delay  as_delay
#if USE_PAR_PATNODE
    '=' -> cmods' cs_par    as_par
#endif
#if USE_PSEQ_PATNODE
    '>' -> cmods' cs_pseq   as_pseq
#endif
#if USE_TRACE_PATNODE
    '+' -> cmods' cs_trace  as_trace
#endif
#if USE_PING_PATNODE
    '^' -> cmods' cs_ping   as_ping
#endif
#if USE_DIE_PATNODE
    '/' -> cmods' cs_die    as_die
#endif
#if USE_TIMING_PATNODE
    '%' -> cmods' cs_timing as_timing
#endif
    _ -> (as, Nothing, s)
   where
    s'@(c:cs) = dropWhile isSpace s
    ( cs_types  , as_types  ) = parse_type_constraints          cs as
    ( cs_delay  , as_delay  ) = parse_delay                     cs as
#if USE_PAR_PATNODE
    ( cs_par    , as_par    ) = ( cs, as { doSpark  = True } )
#endif
#if USE_PSEQ_PATNODE
    ( cs_pseq   , as_pseq   ) = parse_pseq                      cs as
#endif
#if USE_TRACE_PATNODE
    ( cs_trace  , as_trace  ) = ( cs, as { doTrace  = True } )
#endif
#if USE_PING_PATNODE
    ( cs_ping   , as_ping   ) = ( cs, as { doPing   = True } )
#endif
#if USE_DIE_PATNODE
    ( cs_die    , as_die    ) = ( cs, as { doDie    = True } )
#endif
#if USE_TIMING_PATNODE
    ( cs_timing , as_timing ) = ( cs, as { doTiming = True } )
#endif

  parse_type_constraints :: String -> PatNodeAttrs -> (String, PatNodeAttrs)
  parse_type_constraints s'' as
--- | doConstrainType as  = trace "\nwarning: type constraints (:...:) mod given multiple times to a single node, so aggregating type lists." $ (s', as')
   | otherwise           = (s', as')
   where
    s = dropWhile isSpace s''
    as' = as { doConstrainType = True
             , typeConstraints = typeConstraints as ++ tys }
    (tys, s') = f s "" []
    -- Take up to the next ';', ':', or '\\' and deal.
    f :: String -> String -> [String] -> ([String],String)
    f s'' tyacc tysacc
     | null s'    = error "\ncompilePat: type constraint list not ':'-terminated"
     | '\\' == c  = if null cs
                    then f cs (c:tyacc) tysacc
                    else if ':' == head cs    -- note ty is already reversed
                         then f (tail cs) ((':':'\\':ty) ++ tyacc) tysacc
                         else f cs (('\\':ty) ++ tyacc) tysacc
     | ':' == c   = ( reverse $ (reverse $ tyacc ++ ty) : tysacc , dropWhile isSpace cs )
    -- otherwise ';' == c
     | otherwise  = f cs "" $ (reverse $ tyacc ++ ty) : tysacc
     where
      s = dropWhile isSpace s''
      (c:cs) = s'
      (ty',s') = span (\c->c/=';'&&c/=':'&&c/='\\') s
      ty = dropWhile isSpace $ reverse ty'  -- yeah yeah

  parse_delay :: String -> PatNodeAttrs -> (String, PatNodeAttrs)
  parse_delay [] as = error "\nparse_delay: unexpected end of input"
  parse_delay s'' as
--- | doDelay as     = error "\ncompilePat: delay (@) modifier given multiple times to a single node"
--- | doDelay as  = trace "\nwarning: delay (@) mod given multiple times to a single node, so summing." $ (s', as')
   | isNothing mis  = error $ "\nparse_delay: expected a digit 1-9 (not '" ++ [head s] ++ "')"
   | otherwise      = (s', as')
   where
    s = dropWhile isSpace s''
    as' = as { doDelay = True  -- (convenient to set both here)
             , delayus = delayus as + i }
    (mis, s') = parseInt s []
    is = fromJust mis
    i = read is :: Int

#if USE_PSEQ_PATNODE
  parse_pseq :: String -> PatNodeAttrs -> (String, PatNodeAttrs)
  parse_pseq s'' as
   | doPseq as  = error "\ncompilePat: pseq (>) modifier given multiple times to a single node"
   | not ok     = error "\ncompilePat: pseq permutation must cover an initial segment of abc..yz"
-- No harm in allowing it; as for testing arity mismatch, that is not
-- in the parser's purview (at least at this time).  It is easily done
-- as a post-parsing check.
--- | null perm  = error "\ncompilePat: empty pseq permutation"
   | otherwise  = (s', as')
   where
    s = dropWhile isSpace s''
    as' = as { doPseq = True  -- (convenient to set both here)
             , pseqPerm = Just $ map (\c -> ord c - ord 'a') perm }
    (perm, s') = span isLower s
    ok = sort perm == take (length perm) ['a'..'z']
#endif

-------------------------------------------------------------------------------

  -- XXX Doing this to ensure issuance of all warning messages
  -- pertaining to the pattern to be compiled!
  -- Which isn't quite working?!?.... [Never did resolve this.]
  compilePat :: String -> Pattern
#if NFDATA_INSTANCE_PATTERN
  compilePat s = force $ compilePat_ s
--compilePat s = let pat = force $! compilePat_ s in trace (show pat) $! pat
--compilePat s = let pat = force $ compilePat_ s in trace (show pat) $! pat
--compilePat s = let !pat = force $ compilePat_ s in trace (show pat) $ pat
--compilePat s = let pat = force $ compilePat_ s in trace (show pat) $ pat
#else
  compilePat = compilePat_
#endif

  compilePat_ :: String -> Pattern
  compilePat_ str = compilePat' str

-------------------------------------------------------------------------------

  -- | Inverse of 'compilePat'.
  --
  -- @showPat . compilePat patstring  =  patstring@
  --
  -- (up to optional whitespace, and canonical ordering of any attributes),
  -- provided that @compilePat patstring@ succeeds.
{--}
  -- /(And, only up to subpatterns elided from # ('WI' or 'TI') or from * ('WW', 'WN', 'TW', or 'TN') nodes, in case these are still accepted by the parser!)/

  showPat :: Pattern -> String
  showPat (Node pas chs)

    | doDelay  as  = "@" ++ show (delayus as)
                         ++ let as' = as { doDelay = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#if USE_PAR_PATNODE
    | doSpark  as  = "=" ++ let as' = as { doSpark = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
#if USE_PSEQ_PATNODE
    | doPseq   as  = ">" ++ showPerm (pseqPerm as)
                         ++ let as' = as { doPseq  = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
#if USE_TRACE_PATNODE
    | doTrace  as  = "+" ++ let as' = as { doTrace = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
#if USE_PING_PATNODE
    | doPing   as  = "^" ++ let as' = as { doPing  = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
#if USE_DIE_PATNODE
    | doDie    as  = "/" ++ let as' = as { doDie   = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
#if USE_TIMING_PATNODE
    | doTiming as  = "%" ++ let as' = as { doTiming   = False }
                            in showPat (Node (setPatNodeAttrs pas as') chs)
#endif
    | doConstrainType as
       =
--       trace "showPat-doConstraintType HERE!" $
         ":" ++ treps_str
             ++ let as' = as { doConstrainType = False }
                in showPat (Node (setPatNodeAttrs pas as') chs)

#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
    | WI{} <- pas  = "0"   ++ descend chs
#else
    | WI{} <- pas  = "."   ++ descend chs
#endif
    | WR{} <- pas  = ""    ++ descend chs ++ perhapsEmptySubpatterns
#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
    | WS{} <- pas  = "1"   ++ descend chs
#else
    | WS{} <- pas  = "!"   ++ descend chs
#endif
#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_NUMBER_ALONE__SAFE_ONLY_TO_DEPTH_19 || ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
    | WN{} <- pas  =          show n ++ descend chs
#else
    | WN{} <- pas  = "*"   ++ show n ++ descend chs
#endif
#if USE_WW_DEEPSEQ
    | WW{} <- pas  = "*"   ++ descend chs
#endif

#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
    | TI{} <- pas  = "0"   ++ descend chs
#else
    | TI{} <- pas  = "."   ++ descend chs
#endif
    | TR{} <- pas  = ""    ++ descend chs ++ perhapsEmptySubpatterns
--- #if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
---     | TS{} <- pas  = "1"   ++ descend chs
--- #else
---     | TS{} <- pas  = "!"   ++ descend chs
--- #endif
#if ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_NUMBER_ALONE__SAFE_ONLY_TO_DEPTH_19 || ABBREV_WN_AND_TN_CONCRETE_SYNTAX_TO_SINGLE_DIGIT__CAN_ONLY_EXPRESS_DOWN_TO_DEPTH_9
    | TN{} <- pas  =          show n ++ descend chs
#else
    | TN{} <- pas  = "*"   ++ show n ++ descend chs
#endif
#if USE_WW_DEEPSEQ
    | TW{} <- pas  = "*"   ++ descend chs
#endif

   where
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
    emptySubpatterns = "{}"
#else
    emptySubpatterns = "()"
#endif
    perhapsEmptySubpatterns = if null chs then emptySubpatterns else ""
    as = getPatNodeAttrs pas
    n = depth as
    treps = typeConstraints as
    treps_str = intercalate ";" treps ++ ":"

  descend :: [Pattern] -> String
  descend chs
   | null chs = ""
#if USE_CURLY_BRACE_INSTEAD_OF_PAREN_FOR_SUBPATTERNS
   | otherwise = "{" ++ concatMap showPat chs ++ "}"
#else
   | otherwise = "(" ++ concatMap showPat chs ++ ")"
#endif

-------------------------------------------------------------------------------

  parseInt :: String -> String -> ( Maybe String, String )
  parseInt [] acc = ( if null acc then Nothing else Just acc , "" )
  parseInt s@(c:cs) acc
-- These should be safe cutoffs without having to worry about exact figures.
--- DEPTH_USES_INT64 isn't implemented yet, this is just a note
--- for future consideration. (Should be in NFDataN if anywhere...).
--- I'm not ready to make this sweeping change yet.
--- #if DEPTH_USES_INT64
---    | length acc > 19  = error $ "compilePat: * followed by too many (>19) digits"
--- #else
   | length acc > 9  = error $ "compilePat: * followed by too many (>9) digits"
--- #endif
   | isDigit c        = parseInt cs (acc++[c])
   | otherwise        = ( if null acc then Nothing else Just acc , s )

-------------------------------------------------------------------------------

  -- | Using String instead of TypeRep since I wasn't sure
  -- how to avoid mandatory recursion to complete the latter.
  -- (Probably it can be done -- ':~:' perhaps -- but I was
  -- unsure and this is working for the moment.)
  compileTypeReps :: String -> ([String], String)
--compileTypeReps :: String -> ([TypeRep], String)
  compileTypeReps cs = (treps,cs')
   where
    (tnames, cs') = parseTyNames cs
    parseTyNames :: String -> ([String], String)
    parseTyNames s = (sps', s')
     where
      sps' = map (dropWhile pstop) sps
--    !_ = trace ("(sps,s') = " ++ show (sps,s')) ()
      (sps,s') = splitPred psplit pstop s
--    (sps,s') = splitPred p s
      pstop x = x == '{' || x == '}'
--    pstop x = x == '{'
      psplit x = x == ' ' || pstop x
--    p x = x == ' ' || x == '{'
--    p x = not $ isAlphaNum x || x == '_' || x == '\''
#if 1
-- XXX In consideration of the recursion problem with mkTyConApp below,
-- try to use typeOf instead -- but, this won't work! Because we are
-- starting with a String encoding the ...
-- ... or will it? We have to compare two strings; one comes from
-- the user-supplied pattern string we're parsing; the other? We
-- are not "comparing equality" here, it will be done later; we're
-- only compiling a pattern...  So if the treps remain strings
-- in a Pattern, until we're ready to make comparisons; it's
-- inefficient unfortunately, but I feel this will work.
--   More detail: B/c when it comes time to match the pattern,
-- you DO have a concrete value (of some type); it is THEN that
-- you apply (show . typeRepTyCon . typeOf) to it, and then
-- make your Eq String comparison. [This can be optimised later;
-- I'm concerned now with a proof-of-concept, without TH.]
    treps = tnames
#else
    treps = map mktrep tnames
-- XXX You need the recursion for (==) to work; that may not mean
-- we can't use it, but will need some form of pattern-matching,
-- as full equality is going to be disfunctional. (B/c user would
-- have to specify the fully-recursive pattern [when they want to
-- use wildcards or stop nodes down there] -- totally ridiculous.)
--   This could be what :~: is for? (It's recursive, but you perhaps
-- can use in patterns without going full depth?)
-- mkTyConApp (mkTyCon3 "base" "Data.Either" "Either") [typeRep (Proxy::Proxy Bool), typeRep (Proxy::Proxy Int)] == typeRep (Proxy :: Proxy (Either Bool Int))
    mktrep :: String -> TypeRep
    mktrep tname = trep
     where
      tcon = mkTyCon3 "" "" tname
      trep = mkTyConApp tcon []
--mkTyCon3 :: 3xString -> TypeCon
--mkTyConApp :: TyCon -> [TypeRep] -> TypeRep
#endif

-------------------------------------------------------------------------------

  -- Split on the psplit predicate, stop consuming the list
  -- on the pstop predicate.
  splitPred :: (a -> Bool) -> (a -> Bool) -> [a] -> ([[a]], [a])
  splitPred psplit pstop list = splitPred' psplit pstop list []
  splitPred' :: (a -> Bool) -> (a -> Bool) -> [a] -> [[a]] -> ([[a]], [a])
  splitPred' psplit pstop list acc
   | null first  = {-trace "-1-" $-} (acc, rest)
   | null rest   = {-trace "-2-" $-} (acc', [])  -- or (acc, rest), obv.
   | pstop h     = {-trace "-3-" $-} (acc', rest)
   | otherwise   = {-trace "-4-" $-} splitPred' psplit pstop t acc'
   where
    (first,rest) = break psplit list
    (h:t) = rest
    acc' = acc ++ [first]

-------------------------------------------------------------------------------

