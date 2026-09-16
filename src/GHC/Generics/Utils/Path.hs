{-# LANGUAGE UndecidableInstances #-}

-- | Searching for constructors & fields in 'Rep's, and calculating their paths.
module GHC.Generics.Utils.Path where

import GHC.Generics
import Data.Type.Bool ( type If )
import Data.Type.List ( type Reverse )
import Data.Type.Equality ( type (==) )
import Data.Kind ( type Type )
import GHC.TypeError qualified as TE
import GHC.TypeLits ( type Symbol )

-- | Which subtree to follow ("turn to take") in a binary tree.
data Turn = TurnLeft | TurnRight

-- | Searches for the given named constructor in a data 'Rep'.
--
-- Must not have a D1 wrapper.
type SearchCstr :: Symbol -> (k -> Type) -> Either TE.ErrorMessage [Turn]
type family SearchCstr name rep where
  -- handle the empty case early so we don't have to keep checking later
  SearchCstr name V1  = Left (TE.Text "type is empty (no constructors)")
  SearchCstr name rep = SearchCstrNoV1 name '[ '( '[], rep)]

type SearchCstrNoV1 :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchCstrNoV1 name zippers where
  -- match on choice first, as there is only 1 leaf node for a given path
  SearchCstrNoV1 name ('(bcs, (l :+: r)) : zippers) =
    SearchCstrNoV1 name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  SearchCstrNoV1 name ('(bcs, (C1 (MetaCons name' _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchCstrNoV1 name zippers)
  SearchCstrNoV1 name '[] = Left (TE.Text "no matching constructor")

-- | Searches for the given named field in a field constructor 'Rep'.
--
-- Must not have a C1 wrapper.
type SearchField :: Symbol -> (k -> Type) -> Either TE.ErrorMessage [Turn]
type family SearchField name rep where
  -- handle the empty case early so we don't have to keep checking later
  SearchField name U1  = Left (TE.Text "constructor is empty (no fields)")
  SearchField name rep = SearchFieldNoU1 name '[ '( '[], rep)]

type SearchFieldNoU1 :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchFieldNoU1 name zippers where
  -- match on choice first, as there is only 1 leaf node for a given path
  SearchFieldNoU1 name ('(bcs, (l :*: r)) : zippers) =
    SearchFieldNoU1 name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  -- found the first field, named: continue, safe to assume all fields are named
  SearchFieldNoU1 name ('(bcs, (S1 (MetaSel (Just name') _ _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchFieldNamed name zippers)
  -- found the first field, not named: fail
  SearchFieldNoU1 name ('(bcs, (S1 (MetaSel Nothing _ _ _) _)) : zippers) =
    Left (TE.Text "type does not have named fields")
  --SearchFieldNoU1 name '[] = TE.Text "unreachable"

-- Assumes that the 'Rep' has named fields.
-- Due to Haskell's syntax, you can determine this by checking a single field,
-- as either all fields are named or all fields are not named.
type SearchFieldNamed :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchFieldNamed name zippers where
  SearchFieldNamed name ('(bcs, (l :*: r)) : zippers) =
    SearchFieldNamed name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  SearchFieldNamed name ('(bcs, (S1 (MetaSel (Just name') _ _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchFieldNamed name zippers)
  SearchFieldNamed name '[] = Left (TE.Text "no matching field")
