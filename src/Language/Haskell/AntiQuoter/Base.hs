{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Base module for `AntiQuoter`s, which transform parsed syntax into
-- template-haskell syntax to make `QuasiQuoter`s.
module Language.Haskell.AntiQuoter.Base(
    -- * AntiQuoters
    AntiQuoterPass,
    AntiQuoter,
    mkQuasiQuoter,
    AQResult,
    fromPass, (<<>), (<>>),
    -- ** Convenience reexport
    -- | WARNING: when combining AntiQuoter(Pass)es using `extQ` only the
    -- last (rightmost) pass will be used for any source type. The `<<>`
    -- and `<>>` don't suffer from this problem.
    extQ,
    -- ** Internals
    WrappedAQResult(..),
) where

import Control.Monad
import Data.Generics
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Quote

infixl 1 <<>
infixr 2 <>>

-- | Result of an `AntiQuoterPass`
type AQResult q = Maybe (Q q)

-- | Wrapper for `AQResult`, needed for the typechecker.
newtype WrappedAQResult q = AQRW { unAQRW :: AQResult q }

-- | A single quotation pass possibly transforming an @e@ into a @q@.
type AntiQuoterPass e q = e -> Maybe (Q q)

-- | An `AntiQuoter` is the combination of several `AntiQuoterPass`es, trying
-- each of them in order until one passes.
newtype AntiQuoter q = AQ
    { antiQuote :: forall e. Typeable e => AntiQuoterPass e q }

-- | Combining `AntiQuoter`s using `mappend` is left biased.
instance Monoid (AntiQuoter q) where
    mempty = AQ $ const Nothing
    (AQ f) `mappend` (AQ g) = AQ $ \e -> f e `mplus` g e


-- | Create an `AntiQuoter` from an single pass.
fromPass :: Typeable e => AntiQuoterPass e q -> AntiQuoter q
fromPass aqp = AQ $ mkQ Nothing aqp

-- | Combine an existing `AntiQuoter` with an extra pass, where the extra pass
-- is tried if the current quoter fails.
(<<>) :: Typeable e => AntiQuoter q -> AntiQuoterPass e q -> AntiQuoter q
aq <<> aqp = aq `mappend` fromPass aqp
-- | Like `<>>` with flipped arguments, but also trying the extra pass before
-- the quoter.
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
