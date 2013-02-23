{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.AntiQuoter.Base(
    -- * AntiQuoters
    AntiQuoterPass,
    AntiQuoter,
    mkQuasiQuoter, mkEPQuasiQuoter,
    fromPass, (<<>), (<>>),
    -- ** Specialized quoters
    EPAntiQuoter,
    -- * Template syntax class
    EP(..),
    epPass, epPass', epPass'', epDiffer,
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

-- | As `mkQuasiQuoter` but uses an generalized `AntiQuoter`.
mkEPQuasiQuoter :: Data a
    => (String -> Q a)
    -> EPAntiQuoter
    -> QuasiQuoter
mkEPQuasiQuoter parse aq = mkQuasiQuoter parse (aq, aq)

-- | An `AntiQuoter` that works for `Exp` and `Pat`s.
type EPAntiQuoter       = forall q. EP q => AntiQuoter q
type EPAntiQuoterPass e = forall q. EP q => AntiQuoterPass e q

epPass :: Typeable e => (e -> Maybe (Q Exp)) -> (e -> Maybe (Q Pat))
    -> EPAntiQuoterPass e
epPass pe pp = \e -> unAQR . fromEPV $ EPV (AQR $ pe e) (AQR $ pp e)

epPass' :: Typeable e => (e -> Maybe (Q Exp, Q Pat)) -> EPAntiQuoterPass e
epPass' f = epPass (fmap fst . f) (fmap snd . f)

epPass'' :: Typeable e => (e -> Maybe (Q (Exp, Pat))) -> EPAntiQuoterPass e
epPass'' f = epPass ((fmap $ fmap fst) . f) ((fmap $ fmap snd) . f)

epDiffer :: EP q => Q Exp -> Q Pat -> Q q
epDiffer e p = fromEPV $ EPV e p

-- | The result of a transformation.
newtype AQResult q = AQR { unAQR :: (Maybe (Q q)) }

data EPV f = EPV
    { eep :: f Exp
    , pep :: f Pat
    }

-- | Typeclass with the common constructors of `Exp` and `Pat`, usefull for
-- building `EPAntiQuoter`s.
class EP q where
    var     :: Name -> q
    con     :: Name -> [q] -> q
    lit     :: Lit -> q
    tup     :: [q] -> q
    list    :: [q] -> q
    fromEPV :: EPV f -> f q

instance EP Exp where
    var     = VarE
    con     = foldl AppE . ConE
    lit     = LitE
    tup     = TupE
    list    = ListE
    fromEPV = eep
instance EP Pat where
    var     = VarP
    con     = ConP
    lit     = LitP
    tup     = TupP
    list    = ListP
    fromEPV = pep
