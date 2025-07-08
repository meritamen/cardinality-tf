{-# LANGUAGE TemplateHaskell #-}

module Cardinality.TH
  ( genTupleInstances
  ) where

import Cardinality.Internal
import Language.Haskell.TH

genTupleInstances :: Int -> Q [Dec]
genTupleInstances nMax = fmap concat $ mapM genTupleInstance [2..nMax]
  where
    genTupleInstance n = do
      let tvs = (\i -> mkName [ ['a'..'z'] !! (i-1) ]) <$> [1..n]
          ctx = (\tv -> AppT (ConT ''TypeCardinality) (VarT tv)) <$> tvs
          tupleType = foldl AppT (TupleT n) (map VarT tvs)
          cardOfs = (\tv -> AppT (ConT ''CardinalityOf) (VarT tv)) <$> tvs
          rhs = foldl1 (\a b -> AppT (AppT (ConT ''(|*|)) a) b) cardOfs
          assocType = TySynInstD $ TySynEqn Nothing (AppT (ConT ''CardinalityOf) tupleType) rhs
          inst = InstanceD Nothing ctx (AppT (ConT ''TypeCardinality) tupleType) [assocType]
      return [inst]
