{-# LANGUAGE UndecidableInstances #-}

{- | Index trees for convenient constructor/field indexing on generic 'Rep's.

'Rep' exposes constructor and field order. One may choose to use this in generic
functions, e.g. using constructor index as the tag for serializing.
In such cases, authors tend to calculate index in-place.
Sometimes, when writing functions using "GHC.Generics", you may find yourself
calculating TODO.

TODO cereal actually cheats! It calculates top-level L&R sizes, and counts up
from 0 using @(\s -> s `unsafeShiftR` 1)@ etc. based on L1/R1 turns.
That's assuming specific shape.

-}

module GHC.Generics.Utils.Index where

import GHC.Generics
import GHC.Generics.Utils.Path ( Turn(..) )
import GHC.Generics.Utils.Count ( type CountCstrs, type CountFields )
import GHC.TypeLits ( type Natural, type (+), type (-) )
import GHC.TypeNats.Bits ( type ShiftR )
import GHC.TypeError qualified as TE
import GHC.TypeError ( type TypeError )
import Data.Kind ( type Type )

-- | Binary tree with data at leaves.
--
-- Looks similar to type-level 'Rep' constructor sum & field product.
data Tree a
  = Node (Tree a) (Tree a) -- ^ l ':+:' r
  | Leaf a                 -- ^ C1

-- | Binary tree with 'Natural's at leaves.
--
-- Corresponds to type-level 'Rep' constructor sum & field product.
-- Without the "empty" cases ('V1', 'U1').
type IdxTree = Tree Natural

-- | Calculate the left-to-right numeric index for each constructor in a 'Rep',
--   and place in a structurally identical tree (which I call the index tree).
--   The 'Rep' must be at constructor sum level (i.e. must have stripped 'D1').
--
-- 0-indexed. Also returns the total number of constructors (1-indexed).
-- Trivial to calculate from the index tree, but free for us to return it.
--
-- Returns a tree of 'Natural's, with identical structure to the input 'Rep' (or
-- more specifically, the generic constructor sum, ':+:' and 'C1').
-- To use this, you should traverse it at the same time as you do the
-- corresponding 'Rep'.
type IdxTreeCstrs :: (k -> Type) -> (IdxTree, Natural)
type family IdxTreeCstrs rep where
  -- | Empty data type: 0 constructors, erroring cstr tree.
  --
  -- In regular usage, this should not result in a type error, as if you see
  -- a 'V1', you shouldn't pattern match on the cstr tree.
  IdxTreeCstrs V1 = '(TypeError (TE.Text "Bad"), 0)

  -- | Non-empty data type: start indexing algorithm
  IdxTreeCstrs rep = IdxTreeCstrs' 0 '[ '[rep]] '[]

-- | Number cstrs in a 'Rep'.
--
-- To build a corresponding tree, I track depth as I traverse.
-- I do this by using another layer of lists. A list represents a single depth
-- of a subtree. When this tree is empty, we know that we're effectively moving
-- "up". This doesn't matter for traversal, but it does for building a tree!
--
-- Is there a better algorithm for calculating this? I'm not certain.
-- This is busy, but feels natural.
--
-- Note that this does _not_ exploit GHC lays out 'Rep' cstr sums (i.e. it
-- should balance them). If one wanted, one could check if GHC generates
-- consistent reps, and exploit those rules to build an index tree just based
-- off size. But! That still requires counting cstrs, which is half of this.
type IdxTreeCstrs' :: Natural -> [[k -> Type]] -> [IdxTree] -> (IdxTree, Natural)
type family IdxTreeCstrs' count reps idxs where
  -- next element in current depth is a cstr choice: add another depth
  IdxTreeCstrs' count (((l :+: r) : reps) : repss) idxs =
    IdxTreeCstrs' count ('[l, r] : reps : repss) idxs

  -- next element in current depth is a cstr: mark index, increment
  IdxTreeCstrs' count ((C1 _ _ : reps) : repss) idxs =
    IdxTreeCstrs' (count+1) (reps : repss) (Leaf count : idxs)

  -- finished current depth: pop 2 from index stack
  IdxTreeCstrs' count ('[] : repss) (r : l : idxs) =
    IdxTreeCstrs' count repss (Node l r : idxs)

  -- done: well-formed tree implies only 1 element in index stack
  IdxTreeCstrs' count '[ '[]] '[idxs] = '(idxs, count)

-- | Get the total number of leaves in an index tree.
--
-- You should not need to use this, as the indexing algorithm should return it.
type IdxTreeCount :: IdxTree -> Natural
type family IdxTreeCount idxs where
  -- | Node: follow right
  IdxTreeCount (Node l r) = IdxTreeCount r

  -- | Leaf: guaranteed to be rightmost = highest index -> +1 for total count
  IdxTreeCount (Leaf idx) = idx + 1

data IdxState' nat = IdxState
  { idx  :: nat -- ^ Index in full tree.
  , size :: nat -- ^ Size of (i.e. number of leaves in) current subtree.
  }
type IdxState = IdxState' Natural

-- Must not have a D1 wrapper.
type IdxStateInitCstrs :: (k -> Type) -> IdxState
type IdxStateInitCstrs rep = 'IdxState 0 (CountCstrs rep)

-- Must not have a C1 wrapper.
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

type Idx :: IdxState -> Natural
type family Idx state where
  Idx ('IdxState idx size) = idx
  -- Note: We could match on size == 1.

-- shortcut calculate by cheating, assuming specific Rep layout by GHC
-- pretty cheap. needs total cstrs count, and is O(log2n) with meh const factor
-- derived from cereal (PutSum specifically)
-- size denotes the size of the subtree i.e. number of constructors remaining.
-- TODO seems to work, but I should triple check with various shapes!
-- the key is that GHC is is right-heavy, and you can get the right behaviour
-- using right shifts.
type IdxTurnsCheat :: Natural -> Natural -> [Turn] -> Natural
type family IdxTurnsCheat idx size turns where
  IdxTurnsCheat idx size (TurnLeft : turns) =
    -- left  turn: same index, continue left
    IdxTurnsCheat  idx               (SizeL size) turns
  IdxTurnsCheat idx size (TurnRight : turns) =
    -- right turn: increase index by size of left tree, continue right
    IdxTurnsCheat (idx + SizeL size) (SizeR size) turns
  IdxTurnsCheat idx size '[] = idx
-- | Size of left tree
type SizeL size = size `ShiftR` 1
-- | Size of right tree
type SizeR size = size - SizeL size
-- ^ I should write some type synonyms that help authors use this inline.
--   Make a little type containing idx and size, and type families for handling a
--   single turn. The author should use the appropriate one for each instance.
--   Then anyone using my cstr paths can use this.
