{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Patch (ReadKnownError(..), readKnownConstant, throwReadKnownErrorWithCause) where

import Control.Monad.Except
import Control.Lens

import PlutusCore.Core
import PlutusCore.Evaluation.Machine.Exception
import PlutusCore.Evaluation.Result
import PlutusCore.Builtin hiding (readKnownConstant)
import Universe

import Data.String

import GHC.Exts (oneShot)

data ReadKnownError
    = ReadKnownUnliftingError UnliftingError
    | ReadKnownEvaluationFailure
    deriving stock (Eq)

makeClassyPrisms ''ReadKnownError

-- | Throw a @ErrorWithCause ReadKnownError cause@.
throwReadKnownErrorWithCause
    :: (MonadError (ErrorWithCause err cause) m, AsUnliftingError err, AsEvaluationFailure err)
    => ErrorWithCause ReadKnownError cause -> m void
throwReadKnownErrorWithCause (ErrorWithCause rkErr cause) = case rkErr of
    ReadKnownUnliftingError unlErr -> throwingWithCause _UnliftingError unlErr cause
    ReadKnownEvaluationFailure     -> throwingWithCause _EvaluationFailure () cause

instance AsUnliftingError ReadKnownError where
    _UnliftingError = _ReadKnownUnliftingError

instance AsEvaluationFailure ReadKnownError where
    _EvaluationFailure = _EvaluationFailureVia ReadKnownEvaluationFailure

-- See Note [Unlifting values of built-in types].
-- | Convert a constant embedded into a PLC term to the corresponding Haskell value.
readKnownConstant
    :: forall val a cause. KnownBuiltinType val a
    => Maybe cause -> val -> Either (ErrorWithCause ReadKnownError cause) a
-- See Note [Performance of KnownTypeIn instances].
readKnownConstant mayCause val = asConstant mayCause val >>= oneShot \case
    Some (ValueOf uniAct x) -> do
        let uniExp = knownUni @_ @(UniOf val) @a
        case uniAct `geq` uniExp of
            Just Refl -> pure x
            Nothing   -> do
                let err = fromString $ concat
                        [ "Type mismatch: "
                        , "expected: " ++ gshow uniExp
                        , "; actual: " ++ gshow uniAct
                        ]
                throwingWithCause _UnliftingError err mayCause
{-# INLINE readKnownConstant #-}
