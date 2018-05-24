{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Eff.Extra.Fold
  ( foldEffM
  ) where

import           Data.Open.Union (Union)
import           Eff             (Eff, handleRelay)
import qualified Eff.Internal    as EI

-- | Handle all effects at once. Requires poking at Eff's internals.
-- | Probably has quadratic performance, but whatever.
foldEffM :: Monad m => (forall a. Union r a -> m a) -> Eff r b -> m b
foldEffM handleAction = \case
  EI.Val x -> pure x
  EI.E action cont ->
    handleAction action >>= \x -> foldEffM handleAction (EI.qApp cont x)
