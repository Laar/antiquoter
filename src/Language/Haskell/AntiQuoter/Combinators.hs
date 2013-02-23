module Language.Haskell.AntiQuoter.Combinators(
    -- * Lifted constructors.
    varQ, conQ, litQ, tupQ, listQ,

    -- * Unsorted
    splice,
) where

import Language.Haskell.AntiQuoter.Base

import Language.Haskell.TH

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
