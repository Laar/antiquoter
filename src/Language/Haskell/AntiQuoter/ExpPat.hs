-- | Tools for writing one `AntiQuoter` which can be used for both expressions
-- and patterns, thereby reducing copy-and-paste programming.
--
-- For example in the original paper on quasi quoting <http://www.eecs.harvard.edu/~mainland/ghc-quasiquoting/mainland07quasiquoting.pdf>
-- antiquoting is demonstrated by defining the following antiquoters:
--
-- @
-- antiVarE :: Var -> Maybe ExpQ
-- antiVarE (AV v ) = Just $ varE $ mkName v
-- antiVarE _ = Nothing
-- antiVarP :: Var -> Maybe PatQ
-- antiVarP (AV v ) = Just $ varP $ mkName v
-- antiVarP _ = Nothing
-- @
--
-- and a simmilar pair for antiquoting variables. The problem is that the
-- definition for the pattern antiquoter is almost a duplicate of the one for
-- expressions. This similarity between antiquoting expressions and patterns
-- is captured in the `EP` class which can be used to write antiquoters which
-- can yield both expressions and patterns. Using the combinators defined on
-- top of this class (see "Language.Haskell.AntiQuoter.Combinators") the
-- example can be rewritten as
--
-- @
-- antiVar :: EP q => Var ->  Maybe (Q q) -- equivalent to antiVar :: EPAntiQuoterPass Var
-- antiVar (AV v) = Just $ varQ $ mkName v
-- antiVar _      = Nothing
-- @

{-# LANGUAGE RankNTypes #-}
module Language.Haskell.AntiQuoter.ExpPat (

    -- * Template syntax class
    EP(..), EPAntiQuoter, EPAntiQuoterPass,
    mkEPQuasiQuoter,
    epPass, epPass', epPass'',
    epResult, epValue, epPure,

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

-- | Typeclass with the common constructors of `Exp` and `Pat`, useful for
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
mkEPQuasiQuoter parse aq = mkQuasiQuoter parse aq aq

-- | An `AntiQuoter` that works for `Exp` and `Pat`s.
type EPAntiQuoter       = forall q. EP q => AntiQuoter q
type EPAntiQuoterPass e = forall q. EP q => AntiQuoterPass e q

-- | Combine two `AntiQuoterPass`es, one for expression context and another for
-- pattern context, into a single pass for an expression of patter context.
epPass :: Typeable e => AntiQuoterPass e Exp -> AntiQuoterPass e Pat
    -> EPAntiQuoterPass e
epPass pe pp = \e -> epResult (pe e) (pp e)

-- | See `epPass`.
epPass' :: Typeable e => (e -> Maybe (Q Exp, Q Pat)) -> EPAntiQuoterPass e
epPass' f = epPass (fmap fst . f) (fmap snd . f)

-- | See `epPass`.
epPass'' :: Typeable e => AntiQuoterPass e (Exp, Pat) -> EPAntiQuoterPass e
epPass'' f = epPass ((fmap $ fmap fst) . f) ((fmap $ fmap snd) . f)

-- | Make a context dependent result for expression and pattern contexts.
epResult :: EP q => AQResult Exp -> AQResult Pat -> AQResult q
epResult e p = unAQRW . fromEPV $ EPV (AQRW e) (AQRW p)

-- | Make an context dependent value for expression and pattern contexts.
epValue :: EP q => Q Exp -> Q Pat -> Q q
epValue e p = fromEPV $ EPV e p

newtype Identity a = Identity { runIdentity :: a }

-- | Constructs an `EP` value by choosing from an `Exp` of `Pat` as
-- appropriate in the context.
epPure :: EP q => Exp -> Pat -> q
epPure e p = runIdentity . fromEPV $ EPV (Identity e) (Identity p)

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
