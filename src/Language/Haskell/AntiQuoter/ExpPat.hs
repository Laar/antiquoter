-- | Tools for writing one `AntiQuoter` which can be used for both expressions
-- and patterns, thereby reducing copy-paste programming.
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.AntiQuoter.ExpPat (

    -- * Template syntax class
    EP(..), EPAntiQuoter, EPAntiQuoterPass,
    mkEPQuasiQuoter,
    epPass, epPass', epPass'', epDiffer,

    -- ** Internal
    EPV(..),
) where

import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.AntiQuoter.Base


-- | Container for a @f@ of both an `Exp` and a `Pat`. Used internally when the
-- result for `Exp` and `Pat` differ.
data EPV f = EPV
    { eep :: f Exp
    , pep :: f Pat
    }

-- | Typeclass with the common constructors of `Exp` and `Pat`, usefull for
-- building `EPAntiQuoter`s.
class EP q where
    -- | Variable
    var     :: Name -> q
    -- | Constructor with arguments
    con     :: Name -> [q] -> q
    -- | Literal value
    lit     :: Lit -> q
    -- | Tuple
    tup     :: [q] -> q
    -- | List
    list    :: [q] -> q
    -- | Internal unwrapper when the implementation for `Exp` and `Pat` should
    -- differ.
    fromEPV :: EPV f -> f q

-- | As `mkQuasiQuoter` but uses an generalized `AntiQuoter`.
mkEPQuasiQuoter :: Data a
    => (String -> Q a)
    -> EPAntiQuoter
    -> QuasiQuoter
mkEPQuasiQuoter parse aq = mkQuasiQuoter parse (aq, aq)

-- | An `AntiQuoter` that works for `Exp` and `Pat`s.
type EPAntiQuoter       = forall q. EP q => AntiQuoter q
type EPAntiQuoterPass e = forall q. EP q => AntiQuoterPass e q

-- | Combine two `AntiQuoterPass`es, one for expression context and another for
-- pattern context, into a single pass for an expression of patter context.
epPass :: Typeable e => AntiQuoterPass e Exp -> AntiQuoterPass e Pat
    -> EPAntiQuoterPass e
epPass pe pp = \e -> unAQRW . fromEPV $ EPV (AQRW $ pe e) (AQRW $ pp e)

-- | See `epPass`.
epPass' :: Typeable e => (e -> Maybe (Q Exp, Q Pat)) -> EPAntiQuoterPass e
epPass' f = epPass (fmap fst . f) (fmap snd . f)

-- | See `epPass`.
epPass'' :: Typeable e => AntiQuoterPass e (Exp, Pat) -> EPAntiQuoterPass e
epPass'' f = epPass ((fmap $ fmap fst) . f) ((fmap $ fmap snd) . f)

-- | Combine two results, an `Exp` and a `Pat` into a context dependent choice.
epDiffer :: EP q => Q Exp -> Q Pat -> Q q
epDiffer e p = fromEPV $ EPV e p

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
