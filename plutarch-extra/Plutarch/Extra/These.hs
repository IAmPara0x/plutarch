{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.These (PTheseData (..)) where

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import           Plutarch.DataRepr       ( PIsDataReprInstances(PIsDataReprInstances)
                                         )
import Plutarch.Prelude

data PTheseData (a :: PType) (b :: PType) (s :: S)
  = PDThis (Term s (PDataRecord '["_0" ':= a]))
  | PDThat (Term s (PDataRecord '["_0" ':= b]))
  | PDThese (Term s (PDataRecord '["_0" ':= a, "_1" ':= b]))
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (PTheseData a b)
