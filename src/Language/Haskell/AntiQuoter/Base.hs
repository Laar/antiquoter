{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.AntiQuoter.Base(
    -- * AntiQuoters
    AntiQuoterPass,
    AntiQuoter,
    mkQuasiQuoter,
    fromPass, (<<>), (<>>),
    -- ** Internals
    AQResult(..),
) where

import Control.Monad
import Data.Generics
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Quote

infixl 1 <<>
infixr 2 <>>

-- | Wrapper for the result of an `AntiQuoterPass`, for internal use.
newtype AQResult q = AQR { unAQR :: (Maybe (Q q)) }

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
