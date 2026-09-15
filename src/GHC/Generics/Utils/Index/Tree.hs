{-# LANGUAGE UndecidableInstances #-}

{- | Index trees for convenient constructor/field indexing on generic 'Rep's.

This module works on both balanced and non-balanced constructor/field trees.
The "GHC.Generics" Haddocks state

> users /should not rely on a specific nesting strategy/ for :+: and :*:
> being used. The compiler is free to choose any nesting it prefers.

However, GHC has always generated balanced trees with consistent structure.
Such trees permit calculating index faster, skipping full counting.
Many popular libraries exploit this e.g. cereal.
I recommend doing the same, and trusting that GHC will not change its strategy.
For that reason, I recommend using "GHC.Generics.Utils.Index.Balanced".
Regardless, this module is provided for posterity, and because it's neat.

-}

module GHC.Generics.Utils.Index.Tree where

import GHC.Generics
import GHC.TypeLits ( type Natural, type (+) )
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
