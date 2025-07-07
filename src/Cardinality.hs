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
  ( Cardinality (..)
  , CardinalityAdd
  , CardinalityMultiply
  , TypeCardinality (..)
  , GCardinalityOf
  , KnownCardinality (..)
  , cardinality
  ) where

import Data.Proxy
import Data.Void
import Data.Kind
import GHC.Generics
import GHC.TypeLits

data Cardinality = FiniteCardinality Nat | InfiniteCardinality

type family CardinalityAdd (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  CardinalityAdd (FiniteCardinality n) (FiniteCardinality m) = FiniteCardinality (n + m)
  CardinalityAdd _ _ = InfiniteCardinality

type family CardinalityMultiply (a :: Cardinality) (b :: Cardinality) :: Cardinality where
  CardinalityMultiply (FiniteCardinality 0) _ = FiniteCardinality 0
  CardinalityMultiply _ (FiniteCardinality 0)  = FiniteCardinality 0
  CardinalityMultiply (FiniteCardinality n) (FiniteCardinality m) = FiniteCardinality (n GHC.TypeLits.* m)
  CardinalityMultiply _ _ = InfiniteCardinality

class TypeCardinality a where
  type CardinalityOf a :: Cardinality
  type CardinalityOf a = GCardinalityOf (Rep a)

instance TypeCardinality Void where
  type CardinalityOf Void = FiniteCardinality 0

instance TypeCardinality () where
  type CardinalityOf () = FiniteCardinality 1

instance TypeCardinality Bool where
  type CardinalityOf Bool = FiniteCardinality 2

instance TypeCardinality Char where
  type CardinalityOf Char = FiniteCardinality 1114112

instance TypeCardinality a => TypeCardinality (Maybe a) where
  type CardinalityOf (Maybe a) = CardinalityAdd (FiniteCardinality 1) (CardinalityOf a)

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (Either a b) where
  type CardinalityOf (Either a b) = CardinalityAdd (CardinalityOf a) (CardinalityOf b)

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (a, b) where
  type CardinalityOf (a, b) = CardinalityMultiply (CardinalityOf a) (CardinalityOf b)

instance TypeCardinality a => TypeCardinality [a] where
  type CardinalityOf [a] = CardinalityIfZero (CardinalityOf a) (FiniteCardinality 1) InfiniteCardinality

type family CardinalityIfZero (c :: Cardinality) (ifZero :: Cardinality) (ifNonZero :: Cardinality) :: Cardinality where
  CardinalityIfZero (FiniteCardinality 0) ifZero _ = ifZero
  CardinalityIfZero _ _ ifNonZero = ifNonZero

type family GCardinalityOf (f :: Type -> Type) :: Cardinality

type instance GCardinalityOf V1 = FiniteCardinality 0

type instance GCardinalityOf U1 = FiniteCardinality 1

type instance GCardinalityOf (f :+: g) = CardinalityAdd (GCardinalityOf f) (GCardinalityOf g)

type instance GCardinalityOf (f :*: g) = CardinalityMultiply (GCardinalityOf f) (GCardinalityOf g)

type instance GCardinalityOf (C1 c f) = GCardinalityOf f

type instance GCardinalityOf (D1 c f) = GCardinalityOf f

type instance GCardinalityOf (S1 c f) = GCardinalityOf f

type instance GCardinalityOf (Rec0 a) = CardinalityOf a

class KnownCardinality (c :: Cardinality) where
  cardinalityVal :: proxy c -> Either Integer String

instance KnownNat n => KnownCardinality (FiniteCardinality n) where
  cardinalityVal _ = Left $ natVal (Proxy @n)

instance KnownCardinality InfiniteCardinality where
  cardinalityVal _ = Right "infinite"

cardinality :: forall a. KnownCardinality (CardinalityOf a) => Either Integer String
cardinality = cardinalityVal (Proxy :: Proxy (CardinalityOf a))
