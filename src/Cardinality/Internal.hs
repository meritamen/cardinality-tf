{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardinality.Internal
  ( Cardinality (..)
  , Add
  , type (|+|)
  , Substract
  , type (|-|)
  , Multiply
  , type (|*|)
  , Exponent
  , type (|^|)
  , TypeCardinality (..)
  , GCardinalityOf
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

data Cardinality = Finite Nat | Infinite

type family Add (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Add (Finite n) (Finite m) = Finite (n + m)
  Add _ _ = Infinite

type (|+|) (a :: Cardinality) (b :: Cardinality) = Add a b

infixl 6 |+|

type family Substract (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Substract (Finite n) (Finite m) = Finite (n - m)
  Substract Infinite _ = Infinite

type (|-|) (a :: Cardinality) (b :: Cardinality) = Substract a b

infixl 6 |-|

type family Multiply (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Multiply (Finite 0) _ = Finite 0
  Multiply _ (Finite 0)  = Finite 0
  Multiply (Finite n) (Finite m) = Finite (n GHC.TypeLits.* m)
  Multiply _ _ = Infinite

type (|*|) (a :: Cardinality) (b :: Cardinality) = Multiply a b

infixl 7 |*|

type family Exponent (base :: Cardinality) (exponent :: Cardinality) :: Cardinality where
  Exponent (Finite 0) Infinite = Finite 0
  Exponent (Finite 1) Infinite = Finite 1
  Exponent (Finite n) Infinite = Infinite
  Exponent Infinite (Finite _) = Infinite
  Exponent Infinite Infinite = Infinite
  Exponent (Finite n) (Finite 1) = Finite n
  Exponent _ (Finite 0) = Finite 1
  Exponent base exponent = base |*| base |^| (exponent |-| Finite 1)

type (|^|) (base :: Cardinality) (exponent :: Cardinality) = Exponent base exponent

infixr 8 |^|

class TypeCardinality a where
  type CardinalityOf a :: Cardinality
  type CardinalityOf a = GCardinalityOf (Rep a)

type family GCardinalityOf (f :: Type -> Type) :: Cardinality

type instance GCardinalityOf V1 = Finite 0

type instance GCardinalityOf U1 = Finite 1

type instance GCardinalityOf (f :+: g) = GCardinalityOf f |+| GCardinalityOf g

type instance GCardinalityOf (f :*: g) = GCardinalityOf f |*| GCardinalityOf g

type instance GCardinalityOf (C1 c f) = GCardinalityOf f

type instance GCardinalityOf (D1 c f) = GCardinalityOf f

type instance GCardinalityOf (S1 c f) = GCardinalityOf f

type instance GCardinalityOf (Rec0 a) = CardinalityOf a
