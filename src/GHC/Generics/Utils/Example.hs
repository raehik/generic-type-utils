{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}

module GHC.Generics.Utils.Example where

import GHC.Generics.Utils.Path
import GHC.Generics.Utils.Index.Balanced
import GHC.Generics.Utils.Count
import Data.Proxy
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats
import GHC.Generics
--import Data.Kind ( type Type, type Constraint )

-- It works! It works!!!!
genericCstrNameIdx
  :: forall (name :: Symbol)
  -> forall a
  -> (Generic a, GCstrNameIdx name (Rep a))
  => Natural
genericCstrNameIdx name a = gCstrNameIdx @name @(Rep a)

-- v can't do this or k gets made an explicit tyvar >:( just inline...
--type GCstrNameIdx :: Symbol -> (k -> Type) -> Constraint
class GCstrNameIdx (name :: Symbol) rep where gCstrNameIdx :: Natural
instance
  ( SearchCstr name rep ~ Right turns
  , idxState ~ IdxStateInitCstrs rep
  , GCstrNameIdx' turns idxState rep
  ) => GCstrNameIdx name (D1 md rep) where
    gCstrNameIdx = gCstrNameIdx' @turns @idxState @rep

class GCstrNameIdx' turns idxState rep where gCstrNameIdx' :: Natural
instance
  ( GCstrNameIdx' turns (IdxLeft idxState) l
  ) => GCstrNameIdx' (TurnLeft : turns) idxState (l :+: r) where
    gCstrNameIdx' = gCstrNameIdx' @turns @(IdxLeft idxState) @l
instance
  ( GCstrNameIdx' turns (IdxRight idxState) r
  ) => GCstrNameIdx' (TurnRight : turns) idxState (l :+: r) where
    gCstrNameIdx' = gCstrNameIdx' @turns @(IdxRight idxState) @r
instance
  ( KnownNat (Idx idxState)
  ) => GCstrNameIdx' '[] idxState rep where
    gCstrNameIdx' = natVal (Proxy @(Idx idxState))

genericCstrNameIdx2
  :: forall (name :: Symbol)
  -> forall a
  -> (Generic a, GCstrNameIdx2 name (Rep a))
  => Natural
genericCstrNameIdx2 name a = gCstrNameIdx2 @name @(Rep a)

class GCstrNameIdx2 (name :: Symbol) rep where gCstrNameIdx2 :: Natural
instance
  ( SearchCstr name rep ~ Right turns
  , IdxTurnsCstr (CountCstrs rep) turns rep ~ '(repCstr, idxx)
  , KnownNat idxx
  ) => GCstrNameIdx2 name (D1 md rep) where
    gCstrNameIdx2 = natVal (Proxy @idxx)

---

genericFieldNameIdx
  :: forall (name :: Symbol)
  -> forall a
  -> (Generic a, GFieldNameIdx name (Rep a))
  => Natural
genericFieldNameIdx name a = gFieldNameIdx @name @(Rep a)

-- | Only works for data types with 1 constructor!!
class GFieldNameIdx (name :: Symbol) rep where gFieldNameIdx :: Natural
instance
  ( SearchField name rep ~ Right turns
  , idxState ~ IdxStateInitFields rep
  , GFieldNameIdx' turns idxState rep
  ) => GFieldNameIdx name (D1 md (C1 mc rep)) where
    gFieldNameIdx = gFieldNameIdx' @turns @idxState @rep

class GFieldNameIdx' turns idxState rep where gFieldNameIdx' :: Natural
instance
  ( GFieldNameIdx' turns (IdxLeft idxState) l
  ) => GFieldNameIdx' (TurnLeft : turns) idxState (l :*: r) where
    gFieldNameIdx' = gFieldNameIdx' @turns @(IdxLeft idxState) @l
instance
  ( GFieldNameIdx' turns (IdxRight idxState) r
  ) => GFieldNameIdx' (TurnRight : turns) idxState (l :*: r) where
    gFieldNameIdx' = gFieldNameIdx' @turns @(IdxRight idxState) @r
instance
  ( KnownNat (Idx idxState)
  ) => GFieldNameIdx' '[] idxState rep where
    gFieldNameIdx' = natVal (Proxy @(Idx idxState))

{- TODO
In the above cases, we don't even need all these instances.
If you're not doing term-level work, you can just use type families.
My bitfield generics might will do this, right?
The generics just locate the bit.
So, I should provide an example for that.
(And this example should do some term-level work.)
-}
