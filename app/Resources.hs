{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
module Resources where

import Data.Type.Bool
import GHC.TypeLits
import Type.Set

type family AnyIntersection (s1 :: TypeSet k) (s2 :: TypeSet k) :: Bool where
  AnyIntersection Empty _ = False
  AnyIntersection _ Empty = False
  AnyIntersection (Branch a l r) s =
    a `Member` s || (AnyIntersection l s) || (AnyIntersection r s)

type family DisjointUnion (s1 :: TypeSet k) (s2 :: TypeSet k) :: TypeSet k where
  DisjointUnion Empty s = s
  DisjointUnion s Empty = s
  DisjointUnion s1 s2 =
    If (AnyIntersection s1 s2)
    (TypeError (Text "Trying to compose same resources"))
    (Merge s1 s2)

type family EqSet (s1 :: TypeSet k) (s2 :: TypeSet k) :: Bool where
  EqSet Empty Empty = True
  EqSet Empty _     = False
  EqSet _ Empty     = False
  EqSet (Branch a l1 r1) (Branch a l2 r2) = (EqSet l1 l2) && (EqSet r1 r2)
  EqSet (Branch a _ _) (Branch b _ _) = False


type family IsEmpty (s :: TypeSet k) :: Bool where
  IsEmpty Empty = True
  IsEmpty _     = False


class Arrow a where
  arr :: (b -> c) -> a Empty b c
  first :: a r b c -> a r (b,d) (c,d)
  (>>>) :: a r1 b c -> a r2 c d -> a (DisjointUnion r1 r2) b d

class ArrowChoice a where
  unsafeCompose :: a r1 b c -> a r2 c d -> a (Merge r1 r2) b d
