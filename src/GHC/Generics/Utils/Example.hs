{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}

module GHC.Generics.Utils.Example where

import GHC.Generics.Utils.Path ( type SearchCstr, type SearchField )
import GHC.Generics.Utils.Index.Balanced ( type IdxTurnsCstr, type IdxTurnsField )
import GHC.Generics.Utils.Count ( type CountCstrs, type CountFields )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats
import GHC.Generics
--import Data.Kind ( type Type, type Constraint )

genericCstrNameIdx
  :: forall (name :: Symbol)
  -> forall a
  -> (Generic a, GCstrNameIdx name (Rep a))
  => Natural
genericCstrNameIdx name a = gCstrNameIdx @name @(Rep a)

class GCstrNameIdx (name :: Symbol) rep where gCstrNameIdx :: Natural
instance
  ( SearchCstr name rep ~ Right turns
  , IdxTurnsCstr (CountCstrs rep) turns rep ~ '(repCstr, idx)
  , KnownNat idx
  ) => GCstrNameIdx name (D1 md rep) where
    gCstrNameIdx = natVal (Proxy @idx)

---

genericFieldNameIdx
  :: forall (name :: Symbol)
  -> forall a
  -> (Generic a, GFieldNameIdx name (Rep a))
  => Natural
genericFieldNameIdx name a = gFieldNameIdx @name @(Rep a)

class GFieldNameIdx (name :: Symbol) rep where gFieldNameIdx :: Natural
instance
  ( SearchField name rep ~ Right turns
  , IdxTurnsField (CountFields rep) turns rep ~ '(repField, idx)
  , KnownNat idx
  ) => GFieldNameIdx name (D1 md (C1 mc rep)) where
    gFieldNameIdx = natVal (Proxy @idx)

-- TODO provide examples that do term-level work, and can't use the shortcut
