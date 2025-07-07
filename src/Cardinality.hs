{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardinality
  ( TypeCardinality (..)
  , cardinality
  ) where

import Data.Proxy
import Data.Void
import GHC.Generics
import GHC.TypeLits

import Cardinality.Internal
import Cardinality.TH

instance TypeCardinality Void where
  type CardinalityOf Void = Finite 0

instance TypeCardinality () where
  type CardinalityOf () = Finite 1

instance TypeCardinality Bool where
  type CardinalityOf Bool = Finite 2

instance TypeCardinality Char where
  type CardinalityOf Char = Finite 1114112

instance TypeCardinality Integer where
  type CardinalityOf Integer = Infinite

#if defined(x86_64_HOST_ARCH)
instance TypeCardinality Int where
  type CardinalityOf Int = Finite 18446744073709551616
#elif defined(i386_HOST_ARCH)
instance TypeCardinality Int where
  type CardinalityOf Int = Finite 4294967296
#endif

instance TypeCardinality Double where
  type CardinalityOf Double = Finite 18446744073709551616

instance TypeCardinality Float where
  type CardinalityOf Float = Finite 4294967296

instance TypeCardinality a => TypeCardinality (Maybe a) where
  type CardinalityOf (Maybe a) = Finite 1 |+| CardinalityOf a

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (Either a b) where
  type CardinalityOf (Either a b) = CardinalityOf a |+| CardinalityOf b

$(genTupleInstances 12)

instance TypeCardinality a => TypeCardinality [a] where
  type CardinalityOf [a] = CardinalityIfZero (CardinalityOf a) (Finite 1) Infinite

type family CardinalityIfZero (c :: Cardinality) (ifZero :: Cardinality) (ifNonZero :: Cardinality) :: Cardinality where
  CardinalityIfZero (Finite 0) ifZero _ = ifZero
  CardinalityIfZero _ _ ifNonZero = ifNonZero

instance (TypeCardinality a, TypeCardinality b) => TypeCardinality (a -> b) where
  type CardinalityOf (a -> b) = CardinalityOf b |^| CardinalityOf a

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
