{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Extra ( pletC,
                        pmatchC,
                        pletFieldsC,
                        ptraceC,
                        pguardC,
                        pguardC',
                      ) where

import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.DataRepr.Internal.Field
import Plutarch.Internal
import Plutarch.Internal.Other
import Plutarch.String
import Plutarch.TermCont
import Plutarch.Trace

-- | Like `plet` but works in a `TermCont` monad
pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
pmatchC :: PMatch a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

-- | Like `pletFields` but works in a `TermCont` monad.
pletFieldsC ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ PFields a
  , bs ~ Bindings ps fs
  , BindFields ps bs
  ) =>
  Term s a ->
  TermCont @b s (HRec (BoundTerms ps bs s))
pletFieldsC x = tcont $ pletFields @fs x

{- | Like `ptrace` but works in a `TermCont` monad.

=== Example ===

@
foo :: Term s PUnit
foo = unTermCont $ do
  ptraceC "returning unit!"
  pure $ pconstant ()
@
-}
ptraceC :: Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptrace s (f ())

{- | Trace a message and raise error if 'cond' is false. Otherwise, continue.

=== Example ===

@
onlyAllow42 :: Term s (PInteger :--> PUnit)
onlyAllow42 = plam $ \i -> unTermCont $ do
  pguardC "expected 42" $ i #== 42
  pure $ pconstant ()
@
-}
pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

{- | Stop computation and return given term if 'cond' is false. Otherwise, continue.

=== Example ===

@
is42 :: Term s (PInteger :--> PBool)
is42 = plam $ \i -> unTermCont $ do
  pguardC "expected 42" (pconstant False) $ i #== 42
  pure $ pconstant True
@
-}
pguardC' :: Term s a -> Term s PBool -> TermCont @a s ()
pguardC' r cond = tcont $ \f -> pif cond (f ()) r

-- | 'TermCont' producing version of 'ptryFrom'.
