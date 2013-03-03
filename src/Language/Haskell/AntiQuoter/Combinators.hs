{-# LANGUAGE RankNTypes #-}
-- | Several combinators for `AntiQuoters`.
module Language.Haskell.AntiQuoter.Combinators(
    -- * Lifted constructors.
    varQ, conQ, litQ, tupQ, listQ,

    -- * Ignoring
    ignore, ignorePat, onlyExp, ignoreExp, onlyPat,
    -- * Unsorted
    splice, wild,
    nonsenseE, nonsenseP,
) where

import Language.Haskell.TH

import Language.Haskell.AntiQuoter.Base
import Language.Haskell.AntiQuoter.ExpPat

varQ :: EP q => Name -> Q q
varQ = return . var

conQ :: EP q => Name -> [Q q] -> Q q
conQ n = fmap (con n) . sequence

litQ :: EP q => Lit -> Q q
litQ = return . lit

tupQ, listQ :: EP q => [Q q] -> Q q
tupQ  = fmap tup  . sequence
listQ = fmap list . sequence

-- | Uses/Binds a variable of the given name.
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

nonsenseP :: EP q => String -> AQResult Exp -> AQResult q
nonsenseP msg e = e `epResult` (Just $ fail msg)

nonsenseE :: EP q => String -> AQResult Pat -> AQResult q
nonsenseE msg p = (Just $ fail msg) `epResult` p
