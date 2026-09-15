module Test where

import GHC.Generics

data C0
  deriving stock Generic
--data C1 = C1_0
--  deriving stock Generic
data C2 = C2_0 | C2_1
  deriving stock Generic
data C3 = C3_0 | C3_1 | C3_2
  deriving stock Generic
data C4 = C4_0 | C4_1 | C4_2 | C4_3
  deriving stock Generic
data C5 = C5_0 | C5_1 | C5_2 | C5_3 | C5_4
  deriving stock Generic

data C1K1 = C1K1 { c1k1_0 :: () }
  deriving stock Generic
data C1K2 = C1K2 { c1k2_0 :: (), c1k2_1 :: () }
  deriving stock Generic
data C1K3 = C1K3 { c1k3_0 :: (), c1k3_1 :: (), c1k3_2 :: () }
  deriving stock Generic
data C1K4 = C1K4 { c1k4_0 :: (), c1k4_1 :: (), c1k4_2 :: (), c1k4_3 :: () }
  deriving stock Generic
data C1K5 = C1K5 { c1k5_0 :: (), c1k5_1 :: (), c1k5_2 :: (), c1k5_3 :: (), c1k5_4 :: () }
  deriving stock Generic

type TestK5 = TestK5' (Rep C1K5)
type family TestK5' rep where
  TestK5' (D1 _ (C1 _ rep)) = rep

type Rep' a = Rep'' (Rep a)
type family Rep'' rep where
    Rep'' (M1 _ _ rep) = Rep'' rep
    Rep'' (l :+: r) = Rep'' l :+: Rep'' r
    Rep'' (l :*: r) = Rep'' l :*: Rep'' r
    Rep'' (Rec0 a) = U1 -- cheeky reuse for nicer parsing
    Rep'' rep = rep
