{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- GEq and GShow for Union

module Data.Open.Union.Extra where

import           Data.GADT.Compare (GEq, geq)
import           Data.GADT.Show    (GShow, gshowsPrec)
import           Data.Open.Union   (Union, decomp)

instance GEq (Union '[]) where
  geq _ = elimEmptyUnion

instance (GEq f, GEq (Union fs)) => GEq (Union (f ': fs)) where
  geq x y =
    case (decomp x, decomp y) of
      (Right x', Right y') -> geq x' y'
      (Left x', Left y')   -> geq x' y'
      _                    -> Nothing

-- Union should export something like this, but it doesn't.
elimEmptyUnion :: Union '[] x -> a
elimEmptyUnion _ = error "should not happen"

instance GShow (Union '[]) where
  gshowsPrec _ = elimEmptyUnion

instance (GShow f, GShow (Union fs)) => GShow (Union (f : fs)) where
  gshowsPrec p u =
    case decomp u of
      Right x -> gshowsPrec p x
      Left x  -> gshowsPrec p x
