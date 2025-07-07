{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardinality
  ( TypeCardinality (..)
  , cardinality
  ) where

import Data.Proxy
import Data.Void
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

instance TypeCardinality Void where
  type CardinalityOf Void = Finite 0

instance TypeCardinality () where
  type CardinalityOf () = Finite 1

instance TypeCardinality Bool where
  type CardinalityOf Bool = Finite 2

instance TypeCardinality Char where
  type CardinalityOf Char = Finite 1114112

instance TypeCardinality a => TypeCardinality (Maybe a) where
  type CardinalityOf (Maybe a) = Finite 1 |+| CardinalityOf a

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (Either a b) where
  type CardinalityOf (Either a b) = CardinalityOf a |+| CardinalityOf b

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (a, b) where
  type CardinalityOf (a, b) = CardinalityOf a |*| CardinalityOf b

instance TypeCardinality a => TypeCardinality [a] where
  type CardinalityOf [a] = CardinalityIfZero (CardinalityOf a) (Finite 1) Infinite

type family CardinalityIfZero (c :: Cardinality) (ifZero :: Cardinality) (ifNonZero :: Cardinality) :: Cardinality where
  CardinalityIfZero (Finite 0) ifZero _ = ifZero
  CardinalityIfZero _ _ ifNonZero = ifNonZero

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (a -> b) where
  type CardinalityOf (a -> b) = CardinalityOf b |^| CardinalityOf a

type family GCardinalityOf (f :: Type -> Type) :: Cardinality

type instance GCardinalityOf V1 = Finite 0

type instance GCardinalityOf U1 = Finite 1

type instance GCardinalityOf (f :+: g) = GCardinalityOf f |+| GCardinalityOf g

type instance GCardinalityOf (f :*: g) = GCardinalityOf f |*| GCardinalityOf g

type instance GCardinalityOf (C1 c f) = GCardinalityOf f

type instance GCardinalityOf (D1 c f) = GCardinalityOf f

type instance GCardinalityOf (S1 c f) = GCardinalityOf f

type instance GCardinalityOf (Rec0 a) = CardinalityOf a

class KnownCardinality (c :: Cardinality) where
  cardinalityVal :: proxy c -> Either Integer String

instance KnownNat n => KnownCardinality (Finite n) where
  cardinalityVal _ = Left $ natVal (Proxy @n)

instance KnownCardinality Infinite where
  cardinalityVal _ = Right "infinite"

cardinality :: forall a. KnownCardinality (CardinalityOf a) => Either Integer String
cardinality = cardinalityVal (Proxy :: Proxy (CardinalityOf a))
