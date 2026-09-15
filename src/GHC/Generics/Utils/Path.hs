{-# LANGUAGE UndecidableInstances #-}

-- | Searching for constructors & fields in 'Rep's, and calculating their paths.
module GHC.Generics.Utils.Path where

import GHC.Generics
import Data.Type.Bool ( type If )
import Data.Type.Equality ( type (==) )
import Data.Kind ( type Type )
import GHC.TypeError qualified as TE
import GHC.TypeLits ( type Symbol )

-- | Which turn to take at a ':+:' constructor or ':*:' field choice.
data Turn = TurnLeft | TurnRight

type SearchCstr :: Symbol -> (k -> Type) -> Either TE.ErrorMessage [Turn]
type family SearchCstr name rep where
  -- handle the empty case early so we don't have to keep checking later
  SearchCstr name V1 = Left (TE.Text "type is empty (no constructors)")
  SearchCstr name rep = SearchCstrNoV1 name '[ '( '[], rep)]

type SearchCstrNoV1 :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchCstrNoV1 name zippers where
  SearchCstrNoV1 name ('(bcs, (l :+: r)) : zippers) =
    SearchCstrNoV1 name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  SearchCstrNoV1 name ('(bcs, (C1 (MetaCons name' _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchCstrNoV1 name zippers)
  SearchCstrNoV1 name '[] = Left (TE.Text "no matching constructor")

type SearchField :: Symbol -> (k -> Type) -> Either TE.ErrorMessage [Turn]
type family SearchField name rep where
  -- handle the empty case early so we don't have to keep checking later
  SearchField name U1 = Left (TE.Text "constructor is empty (no fields)")
  SearchField name rep = SearchFieldNoU1 name '[ '( '[], rep)]

type SearchFieldNoU1 :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchFieldNoU1 name zippers where
  SearchFieldNoU1 name ('(bcs, (l :*: r)) : zippers) =
    SearchFieldNoU1 name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  SearchFieldNoU1 name ('(bcs, (S1 (MetaSel (Just name') _ _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchFieldValid name zippers)
  SearchFieldNoU1 name ('(bcs, (S1 (MetaSel Nothing _ _ _) _)) : zippers) =
    Left (TE.Text "type does not have named fields")
  --SearchFieldNoU1 name '[] = TE.Text "unreachable"

type SearchFieldValid :: Symbol -> [([Turn], k -> Type)] -> Either TE.ErrorMessage [Turn]
type family SearchFieldValid name zippers where
  SearchFieldValid name ('(bcs, (l :*: r)) : zippers) =
    SearchFieldValid name ('((TurnLeft : bcs), l) : '((TurnRight : bcs), r) : zippers)
  SearchFieldValid name ('(bcs, (S1 (MetaSel (Just name') _ _ _) _)) : zippers) =
    If (name == name') (Right (Reverse bcs)) (SearchFieldValid name zippers)
  SearchFieldValid name '[] = Left (TE.Text "no matching field")

-- | Reverse a type level list.
type Reverse as = Reverse' as '[]
type family Reverse' (as :: [k]) (acc :: [k]) :: [k] where
  Reverse' '[]      acc = acc
  Reverse' (a : as) acc = Reverse' as (a : acc)
