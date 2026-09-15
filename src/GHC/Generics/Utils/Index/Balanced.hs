{-# LANGUAGE UndecidableInstances #-}

{- | Fast type-level index calculation
     on generic 'Rep's with GHC's balanced nesting.

GHC structures multiple constructors and fields in generic 'Rep's using a
right-balanced binary tree i.e. for uneven subtrees, the right tree is largest.
This property can be exploited to calculate the index of a given cstr/field
quick-fast.

Is this naughty? Yes.
GHC explicitly tells you no in the "GHC.Generics" Haddocks:

> users /should not rely on a specific nesting strategy/ for :+: and :*:
> being used. The compiler is free to choose any nesting it prefers.

But as of 2026-09-16, everyone else has already assumed this nesting.
And it /is/ faster. So go ahead.

-}

module GHC.Generics.Utils.Index.Balanced where

import GHC.Generics
import GHC.Generics.Utils.Path ( Turn(..) )
import GHC.Generics.Utils.Count ( type CountCstrs, type CountFields )
import GHC.TypeLits ( type Natural, type (+), type (-) )
import GHC.TypeNats.Bits ( type ShiftR )
import Data.Kind ( type Type )

data IdxState' nat = IdxState
  { idx  :: nat -- ^ Index in full tree.
  , size :: nat -- ^ Size of (i.e. number of leaves in) current subtree.
  }
type IdxState = IdxState' Natural

-- Must not have a D1 wrapper.
-- If you need size in your generics, don't use this, it'll recount.
type IdxStateInitCstrs :: (k -> Type) -> IdxState
type IdxStateInitCstrs rep = 'IdxState 0 (CountCstrs rep)

-- Must not have a C1 wrapper.
-- If you need size in your generics, don't use this, it'll recount.
type IdxStateInitFields :: (k -> Type) -> IdxState
type IdxStateInitFields rep = 'IdxState 0 (CountFields rep)

-- | Turn left in a GHC generic representation binary tree.
type IdxLeft :: IdxState -> IdxState
type family IdxLeft state where
  IdxLeft  ('IdxState idx size) = 'IdxState idx (SizeL size)

-- | Turn left in a GHC generic representation binary tree.
type IdxRight :: IdxState -> IdxState
type family IdxRight state where
  IdxRight ('IdxState idx size) = 'IdxState (idx + SizeL size) (SizeR size)

-- | Size of left tree
type SizeL size = size `ShiftR` 1
-- | Size of right tree
type SizeR size = size - SizeL size

type Idx :: IdxState -> Natural
type family Idx state where
  Idx ('IdxState idx size) = idx
  -- Note: We could match on size == 1.

-- taking size because sometimes we also want it in the calling generics
type IdxTurnsCstr size turns rep = IdxTurnsCstr' 0 size turns rep

type IdxTurnsCstr' :: Natural -> Natural -> [Turn] -> (k -> Type) -> (k -> Type, Natural)
type family IdxTurnsCstr' idx size turns rep where
  IdxTurnsCstr' idx size (TurnLeft  : turns) (l :+: r) =
    -- left  turn: same index, continue left
    IdxTurnsCstr'  idx               (SizeL size) turns l
  IdxTurnsCstr' idx size (TurnRight : turns) (l :+: r) =
    -- right turn: increase index by size of left tree, continue right
    IdxTurnsCstr' (idx + SizeL size) (SizeR size) turns r
  IdxTurnsCstr' idx size '[] rep = '(rep, idx)

-- taking size because sometimes we also want it in the calling generics
type IdxTurnsField size turns rep = IdxTurnsField' 0 size turns rep

type IdxTurnsField' :: Natural -> Natural -> [Turn] -> (k -> Type) -> (k -> Type, Natural)
type family IdxTurnsField' idx size turns rep where
  IdxTurnsField' idx size (TurnLeft  : turns) (l :*: r) =
    -- left  turn: same index, continue left
    IdxTurnsField'  idx               (SizeL size) turns l
  IdxTurnsField' idx size (TurnRight : turns) (l :*: r) =
    -- right turn: increase index by size of left tree, continue right
    IdxTurnsField' (idx + SizeL size) (SizeR size) turns r
  IdxTurnsField' idx size '[] rep = '(rep, idx)
