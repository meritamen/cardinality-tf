{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardinality.Internal
  ( Cardinality (..)
  , CardinalityAdd
  , type (|+|)
  , CardinalitySubstract
  , type (|-|)
  , CardinalityMultiply
  , type (|*|)
  , CardinalityExponentiation
  , type (|^|)
  , TypeCardinality (..)
  , GCardinalityOf
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

data Cardinality = Finite Nat | Infinite

type family CardinalityAdd (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  CardinalityAdd (Finite n) (Finite m) = Finite (n + m)
  CardinalityAdd _ _ = Infinite

type (|+|) (a :: Cardinality) (b :: Cardinality) = CardinalityAdd a b

infixl 6 |+|

type family CardinalitySubstract (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  CardinalitySubstract (Finite n) (Finite m) = Finite (n - m)
  CardinalitySubstract Infinite _ = Infinite

type (|-|) (a :: Cardinality) (b :: Cardinality) = CardinalitySubstract a b

infixl 6 |-|

type family CardinalityMultiply (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  CardinalityMultiply (Finite 0) _ = Finite 0
  CardinalityMultiply _ (Finite 0)  = Finite 0
  CardinalityMultiply (Finite n) (Finite m) = Finite (n GHC.TypeLits.* m)
  CardinalityMultiply _ _ = Infinite

type (|*|) (a :: Cardinality) (b :: Cardinality) = CardinalityMultiply a b

infixl 7 |*|

type family CardinalityExponentiation (base :: Cardinality) (exponent :: Cardinality) :: Cardinality where
  CardinalityExponentiation (Finite 0) Infinite = Finite 0
  CardinalityExponentiation (Finite 1) Infinite = Finite 1
  CardinalityExponentiation (Finite n) Infinite = Infinite
  CardinalityExponentiation Infinite (Finite _) = Infinite
  CardinalityExponentiation Infinite Infinite = Infinite
  CardinalityExponentiation (Finite n) (Finite 1) = Finite n
  CardinalityExponentiation _ (Finite 0) = Finite 1
  CardinalityExponentiation base exponent = base |*| base |^| (exponent |-| Finite 1)

type (|^|) (base :: Cardinality) (exponent :: Cardinality) = CardinalityExponentiation base exponent

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
