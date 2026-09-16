{-# LANGUAGE UndecidableInstances #-}

-- | Counting constructors & fields in generic 'Rep's.
module GHC.Generics.Utils.Count where

import GHC.Generics
import GHC.TypeLits ( type Natural, type (+) )
import Data.Kind ( type Type )

-- | Count the number of constructors in a generic constructor sum
--   (i.e. a data type).
--
-- Must not have a D1 wrapper.
type CountCstrs :: (k -> Type) -> Natural
type family CountCstrs rep where
  -- handle the empty case early so we don't have to keep checking later
  CountCstrs V1  = 0
  CountCstrs rep = CountCstrsNoV1 0 '[rep]

type CountCstrsNoV1 :: Natural -> [k -> Type] -> Natural
type family CountCstrsNoV1 n rep where
  -- match on choice first, as there is only 1 leaf node for a given path
  CountCstrsNoV1 n ((l :+: r) : reps) = CountCstrsNoV1 n     (l : r : reps)
  CountCstrsNoV1 n (C1 _ _    : reps) = CountCstrsNoV1 (n+1)          reps
  CountCstrsNoV1 n '[]                = n

-- | Count the number of fields in a generic field sum (i.e. a constructor).
--
-- Must not have a C1 wrapper.
type CountFields :: (k -> Type) -> Natural
type family CountFields rep where
  -- handle the empty case early so we don't have to keep checking later
  CountFields U1  = 0
  CountFields rep = CountFieldsNoU1 0 '[rep]

type CountFieldsNoU1 :: Natural -> [k -> Type] -> Natural
type family CountFieldsNoU1 n rep where
  -- match on choice first, as there is only 1 leaf node for a given path
  CountFieldsNoU1 n ((l :*: r) : reps) = CountFieldsNoU1 n     (l : r : reps)
  CountFieldsNoU1 n (S1 _ _    : reps) = CountFieldsNoU1 (n+1)          reps
  CountFieldsNoU1 n '[]                = n
