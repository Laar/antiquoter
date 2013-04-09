{-# LANGUAGE RankNTypes #-}
-- | Several combinators for `AntiQuoters`, mainly `EP` related.
module Language.Haskell.AntiQuoter.Combinators(
    -- * Lifted constructors.
    varQ, conQ, litQ, tupQ, listQ,

    -- * Ignoring
    ignore, ignorePat, onlyExp, ignoreExp, onlyPat,
    -- * Unsorted
    splice, wild,
    nonsenseP, nonsenseE,
) where

import Language.Haskell.TH

import Language.Haskell.AntiQuoter.Base
import Language.Haskell.AntiQuoter.ExpPat

-- | Generalized `varE`/`varP`.
varQ :: EP q => Name -> Q q
varQ = return . var

-- | Generalized `conP` or combination of `conE` and `appE`.
conQ :: EP q => Name -> [Q q] -> Q q
conQ n = fmap (con n) . sequence

-- | Generalized `litE`/`litP`.
litQ :: EP q => Lit -> Q q
litQ = return . lit

tupQ, listQ :: EP q => [Q q] -> Q q
-- | Generalized `tupE`/`tupP`.
tupQ  = fmap tup  . sequence
-- | Generalized `listE`/`listP`.
listQ = fmap list . sequence

-- | Uses/Binds a variable of the given name.
--
-- >  splice = varQ . mkName
--
splice :: EP q => String -> Q q
splice =  varQ . mkName

-- | Use a wildcard in pattern context and the given expression in expression
-- contexts. Consider for example the folowing constructor
--
-- > EX SrcLoc OtherType
--
-- When pattern matching the pattern should look like @EX _ x@, using a
-- wildcard for the source location. On the other hand making an expression
-- should use some result say resulting in @EX someSrcLoc x@. With the `wild`
-- function this general quoter can be written as
--
-- > con ''EX [wild someSrcLoc', splice "x"]
--
-- Assuming that @someSrcLoc' :: ExpQ@ and that its result is of type @SrcLoc@.
wild :: EP q => Q Exp -> Q q
wild e = epValue e wildP

-- | An results that does not output anything.
ignore :: AQResult q
ignore = Nothing

-- | Yielding only a result for expressions and ignoring in patterns.
ignorePat, onlyExp :: EP q => AQResult Exp -> AQResult q
ignorePat e = epResult e ignore
-- | Alias for `ignorePat`.
onlyExp = ignorePat

-- | Yielding only a result for patterns and ignoring in expressions.
ignoreExp, onlyPat :: EP q => AQResult Pat -> AQResult q
ignoreExp = epResult ignore
-- | Alias for `ignoreExp`.
onlyPat = ignoreExp

-- | A `fail`ing result for patterns, useful for when a the pattern matched by
-- the using functions should never happen when antiquoting patterns.
nonsenseP :: EP q => String -> AQResult Exp -> AQResult q
nonsenseP msg e = e `epResult` (Just $ fail msg)

-- | See `nonsenseP` but failing on expresions.
nonsenseE :: EP q => String -> AQResult Pat -> AQResult q
nonsenseE msg p = (Just $ fail msg) `epResult` p
