-- | Several combinators for `AntiQuoters`.
module Language.Haskell.AntiQuoter.Combinators(
    -- * Lifted constructors.
    varQ, conQ, litQ, tupQ, listQ,

    -- * Unsorted
    splice, wild,
) where


import Language.Haskell.TH

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
wild e = epDiffer e wildP
