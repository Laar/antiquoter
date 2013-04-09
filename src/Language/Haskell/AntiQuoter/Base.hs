{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{- | Base module for `AntiQuoter`s, defining some basic type-aliases and and
combinators for antiquoting.

To for examples in the documentation of this library the following data
types defining the untyped lambda calculus syntax:

@
data Expr
    = VarE Var
    | Lam  Var Expr
    | App  Expr Expr
    | AntiExpr String
    deriving (Typeable, Data)
data Var
    = Var     String
    | AntiVar String
    deriving (Typeable, Data)
@

(note: the idea for using lambda calculus comes from the original paper on
quasi-quoting <http://www.eecs.harvard.edu/~mainland/ghc-quasiquoting/mainland07quasiquoting.pdf>)

A simple quasi-quoter without support for antiquoting can be defined by:

@
lExp = QuasiQuoter
    { quoteExp  = dataToExpQ (const Nothing) . parseExpr
    , quotePat  = dataToPatQ (const Nothing) . parseExpr
    , quoteType = error \"No type quoter\"
    , quoteDec  = error \"No declaration quoter\"
    }
parseExpr :: String -> Expr
parseExpr = undefined -- implementation omitted
@

Now to add antiquoting it is needed to treat the AntiExpr and AntiVar
constructors as special and translate them ourselves. This introduces an
@`AntiQuoterPass` e p@, which is a specific translation rule from source syntax
@e@ to template haskell type @p@. In the example this can be used to defined:

@
antiExprE :: AntiQuoterPass Expr Exp
antiExprE (AntiExpr s) = Just . varE $ mkName s
antiExprE _            = Nothing
antiVarE :: AntiQuoterPass Var Exp
antiVarE (AntiVar s) = Just . varE $ mkName s
antiVarE _           = Nothing

antiExprP :: AntiQuoterPass Expr Pat -- implementation simmilar to antiExprE
antiVarP  :: AntiQuoterPass Var  Pat -- implementation simmilar to antiVarE
@

Both rules should be used when antiquoting as an exception to the base case
(using the default translation, @const Nothing@). Which can be done using
@(`<>>`)@, creating an `AntiQuoter`. Where an `AntiQuoter` represents a
combination of `AntiQuoterPass`es which can be used to antiquote multiple
datatypes. In the example this would result in

@
lExp = QuasiQuoter
    { quoteExp  = dataToExpQ antiE . parseExpr
    , quotePat  = dataToPatQ antiP . parseExpr
    , quoteType = error \"No type quoter\"
    , quoteDec  = error \"No declaration quoter\"
    }
    where
        antiE :: AntiQuoter Exp
        antiE = antiExprE \<>> antiVarE \<>> const Nothing
        antiP :: AntiQuoter Pat
        antiP = antiExprP \<>> antiVarP \<>> const Nothing
@
-}
module Language.Haskell.AntiQuoter.Base(
    -- * AntiQuoters
    AntiQuoterPass,
    AntiQuoter,
    AQResult,
    -- * Combining AntiQuoters
    mkQuasiQuoter,
    fromPass, (<<>), (<>>),
    -- ** Convenience reexport
    -- | WARNING: when combining AntiQuoter(Pass)es using `extQ` only the
    -- WARNING: when combining AntiQuoter(Pass)es using `extQ` only the
    -- last (rightmost) pass will be used for any source type. The `<<>`
    -- and `<>>` don't suffer from this problem.
    extQ,
    -- ** Internals
    WrappedAQResult(..),
) where

import Control.Monad
import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote

infixl 1 <<>
infixr 2 <>>

-- | A single antiquotation for a specific source type. Usually @e@ is a type
-- from the parsed language and @q@ is the target type (usually `Pat` or
-- `Exp`). A @Just result@ indicates that the input should be antiquoted into
-- @result@ while @Nothing@ indicates that there is no special antiquotation.
type AntiQuoterPass e q = e -> Maybe (Q q)


-- | Result of an `AntiQuoterPass` (AntiQuoterPass e q = e -> AQResult q), see
-- `AntiQuoterPass` on what Nothing and Just mean.
type AQResult q = Maybe (Q q)

-- | Wrapper for `AQResult`, needed for the typechecker.
newtype WrappedAQResult q = AQRW { unAQRW :: AQResult q }


-- | An `AntiQuoter` is the combination of several `AntiQuoterPass`es, trying
-- each of them in order until one passes.
type AntiQuoter q = forall e. Typeable e => AntiQuoterPass e q

-- | Create an `AntiQuoter` from an single pass.
fromPass :: Typeable e => AntiQuoterPass e q -> AntiQuoter q
fromPass aqp = mkQ Nothing aqp

-- | Combine an existing `AntiQuoter` with an extra pass, where the extra pass
-- is tried if the current quoter fails.
(<<>) :: Typeable e => AntiQuoter q -> AntiQuoterPass e q -> AntiQuoter q
aq <<> aqp = \e -> aq e `mplus` fromPass aqp e
-- | Like `<>>` with flipped arguments, but also trying the extra pass before
-- the quoter.
(<>>) :: Typeable e => AntiQuoterPass e q -> AntiQuoter q -> AntiQuoter q
aqp <>> aq = \e -> fromPass aqp e `mplus` aq e

-- | Create an QuasiQuoter for expressions and patterns from a parser and two
-- antiquoters.
mkQuasiQuoter :: Data a
    => (String -> Q a)
    -> AntiQuoter Exp
    -> AntiQuoter Pat
    -> QuasiQuoter
mkQuasiQuoter parse aqExp aqPat
    = QuasiQuoter
    { quoteExp = parse >=> dataToExpQ aqExp
    , quotePat = parse >=> dataToPatQ aqPat
-- To prevent warnings
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = error $ "Language.Haskell.Antiquoter: can't handle types"
    , quoteDec  = error $ "Language.Haskell.Antiquoter: can't handle decls"
#endif
    }
