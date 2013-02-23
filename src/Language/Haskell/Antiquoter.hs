{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.Antiquoter (
    -- * AntiQuoters
    AntiQuoterPass,
    AntiQuoter,
    mkQuasiQuoter, mkEPQuasiQuoter,
    fromPass, (<<>), (<>>),
    -- ** Specialized quoters
    EPAntiQuoter,
    -- * Template syntax class
    EP(..),

    -- ** Lifted versions
    varQ, conQ, litQ, tupQ, listQ,
) where

import Control.Monad
import Data.Generics
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Quote

infixl 1 <<>
infixr 2 <>>

-- | A single quotation pass possibly transforming an @e@ into a @q@.
type AntiQuoterPass e q = e -> Maybe (Q q)

-- | An `AntiQuoter` is the combination of several `AntiQuoterPass`es, trying
-- each of them in order until one passes.
newtype AntiQuoter q = AQ
    { antiQuote :: forall e. Data e => AntiQuoterPass e q }

-- | Combining `AntiQuoter`s using `mappend` is left biased.
instance Monoid (AntiQuoter q) where
    mempty = AQ $ const Nothing
    (AQ f) `mappend` (AQ g) = AQ $ \e -> f e `mplus` g e

-- | An `AntiQuoter` that works for `Exp` and `Pat`s.
type EPAntiQuoter = forall q. EP q => AntiQuoter q

-- | Create an `AntiQuoter` from an single pass.
fromPass :: Typeable e => AntiQuoterPass e q -> AntiQuoter q
fromPass aqp = AQ $ mkQ Nothing aqp

-- | Combine an existing `AntiQuoter` with an extra pass, where the extra pass
-- is tried if the current quoter fails.
(<<>) :: Typeable e => AntiQuoter q -> AntiQuoterPass e q -> AntiQuoter q
aq <<> aqp = aq `mappend` fromPass aqp
-- | As `<>>` but first trying the pass and if it fails try the quoter.
(<>>) :: Typeable e => AntiQuoterPass e q -> AntiQuoter q -> AntiQuoter q
aqp <>> aq = fromPass aqp `mappend` aq

-- | Create an QuasiQuoter for expressions and patterns from a parser and two
-- antiquoters.
mkQuasiQuoter :: Data a
    => (String -> Q a)
    -> (AntiQuoter Exp, AntiQuoter Pat)
    -> QuasiQuoter
mkQuasiQuoter parse (aqExp, aqPat)
    = QuasiQuoter
    { quoteExp = parse >=> dataToExpQ (antiQuote aqExp)
    , quotePat = parse >=> dataToPatQ (antiQuote aqPat)
-- To prevent warnings
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = error $ "Language.Haskell.Antiquoter: can't handle types"
    , quoteDec  = error $ "Language.Haskell.Antiquoter: can't handle decls"
#endif
    }

-- | As `mkQuasiQuoter` but uses an generalized `AntiQuoter`.
mkEPQuasiQuoter :: Data a
    => (String -> Q a)
    -> EPAntiQuoter
    -> QuasiQuoter
mkEPQuasiQuoter parse aq = mkQuasiQuoter parse (aq, aq)

-- | Typeclass with the common constructors of `Exp` and `Pat`, usefull for
-- building `EPAntiQuoter`s.
class EP q where
    var     :: Name -> q
    con     :: Name -> [q] -> q
    lit     :: Lit -> q
    tup     :: [q] -> q
    list    :: [q] -> q

instance EP Exp where
    var     = VarE
    con     = foldl AppE . ConE
    lit     = LitE
    tup     = TupE
    list    = ListE
instance EP Pat where
    var     = VarP
    con     = ConP
    lit     = LitP
    tup     = TupP
    list    = ListP

varQ :: EP q => Name -> Q q
varQ = return . var

conQ :: EP q => Name -> [Q q] -> Q q
conQ n = fmap (con n) . sequence

litQ :: EP q => Lit -> Q q
litQ = return . lit

tupQ, listQ :: EP q => [Q q] -> Q q
tupQ  = fmap tup  . sequence
listQ = fmap list . sequence
