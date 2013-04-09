{-# LANGUAGE RankNTypes #-}
{- | `Exp` and `Pat` are for most part used in simmilar fashion. Most
AntiQuoter(Pass)es have to be written for both datatypes and their
implementation is more or less identical in structure. To reduce copy-and-paste
programming it would be best if it would only need one AntiQuoter(Pass) that
works on both `Exp` and `Pat`.

This module defines the `EP` typeclass expressing the similarity between `Exp`
and `Pat` and some basic functions to use them with `AntiQuoterPass`es. The
"Language.Haskell.AntiQuoter.Combinators" defines the combinator functions on
top of these functions, which are probably more suitable for users.


As an example of the problem take the antiquoters in
"Language.Haskell.AntiQuoter.Base" where there are two AntiQuoterPasses for
each source type, for Var they are

@
antiVarE :: AntiQuoterPass Var Exp
antiVarE (AV v ) = Just $ varE $ mkName v
antiVarE _ = Nothing
antiVarP :: AntiQuoterPass Var Pat
antiVarP (AV v ) = Just $ varP $ mkName v
antiVarP _ = Nothing
@

The problem is that the definition for the pattern antiquoter is almost a
duplicate of the one for expressions. This similarity between antiquoting
expressions and patterns is captured in the `EP` class which can be used to
write antiquoters which an yield both expressions and patterns. Using the
combinators defined on top of this class (see
"Language.Haskell.AntiQuoter.Combinators") the example can be rewritten as

@
antiVar :: EP q => AntiQuoterPass Var q -- equivalent to antiVar :: EPAntiQuoterPass Var
antiVar (AV v) = Just $ varQ $ mkName v
antiVar _      = Nothing
@
-}
module Language.Haskell.AntiQuoter.ExpPat (

    -- * Template syntax class
    EP(..), EPAntiQuoter, EPAntiQuoterPass,
    mkEPQuasiQuoter,
    -- ** Low level functions used when the result for `Exp` and `Pat` differs.
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

-- | An `AntiQuoter` that works for `Exp` and `Pat` results.
type EPAntiQuoter       = forall q. EP q => AntiQuoter q
-- | An `AntiQuoterPass` that works for `Exp` and `Pat` results.
type EPAntiQuoterPass e = forall q. EP q => AntiQuoterPass e q

-- | Combine two `AntiQuoterPass`es, one for expression context and another for
-- pattern context, into a single pass which can be used in both contexts.
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
