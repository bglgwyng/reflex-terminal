module Reflex.Terminal.Prelude (
  module Control.Lens,
  module Control.Monad,
  module Control.Monad.Except,
  module Control.Monad.State.Strict,
  module GHC.Records,
  module Data.Foldable,
  makeLenses',
  spanM,
)
where

import Control.Lens
import Control.Lens.Internal.FieldTH
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable
import GHC.Records
import Language.Haskell.TH

makeLenses' :: Name -> DecsQ
makeLenses' = makeFieldOptics (lensRules{_fieldToDef = \_ _ n -> [TopName (mkName (nameBase n))]})

spanM :: forall m a. (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM predicate = go []
 where
  go :: [a] -> [a] -> m ([a], [a])
  go acc [] = pure (reverse acc, [])
  go acc rest@(x : xs) = do
    matches <- predicate x
    if matches
      then go (x : acc) xs
      else pure (reverse acc, rest)
