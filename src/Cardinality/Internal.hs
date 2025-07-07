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

data Cardinality = Finity Nat | Infinity

type family Add (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Add (Finity n) (Finity m) = Finity (n + m)
  Add _ _ = Infinity

type (|+|) (a :: Cardinality) (b :: Cardinality) = Add a b

infixl 6 |+|

type family Substract (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Substract (Finity n) (Finity m) = Finity (n - m)
  Substract Infinity _ = Infinity

type (|-|) (a :: Cardinality) (b :: Cardinality) = Substract a b

infixl 6 |-|

type family Multiply (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  Multiply (Finity 0) _ = Finity 0
  Multiply _ (Finity 0)  = Finity 0
  Multiply (Finity n) (Finity m) = Finity (n GHC.TypeLits.* m)
  Multiply _ _ = Infinity

type (|*|) (a :: Cardinality) (b :: Cardinality) = Multiply a b

infixl 7 |*|

type family Exponent (base :: Cardinality) (exponent :: Cardinality) :: Cardinality where
  Exponent (Finity 0) Infinity = Finity 0
  Exponent (Finity 1) Infinity = Finity 1
  Exponent (Finity n) Infinity = Infinity
  Exponent Infinity (Finity _) = Infinity
  Exponent Infinity Infinity = Infinity
  Exponent (Finity n) (Finity 1) = Finity n
  Exponent _ (Finity 0) = Finity 1
  Exponent base exponent = base |*| base |^| (exponent |-| Finity 1)

type (|^|) (base :: Cardinality) (exponent :: Cardinality) = Exponent base exponent

infixr 8 |^|

class TypeCardinality a where
  type CardinalityOf a :: Cardinality
  type CardinalityOf a = GCardinalityOf (Rep a)

type family GCardinalityOf (f :: Type -> Type) :: Cardinality

type instance GCardinalityOf V1 = Finity 0

type instance GCardinalityOf U1 = Finity 1

type instance GCardinalityOf (f :+: g) = GCardinalityOf f |+| GCardinalityOf g

type instance GCardinalityOf (f :*: g) = GCardinalityOf f |*| GCardinalityOf g

type instance GCardinalityOf (C1 c f) = GCardinalityOf f

type instance GCardinalityOf (D1 c f) = GCardinalityOf f

type instance GCardinalityOf (S1 c f) = GCardinalityOf f

type instance GCardinalityOf (Rec0 a) = CardinalityOf a
