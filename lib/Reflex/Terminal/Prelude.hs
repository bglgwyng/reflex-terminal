module Reflex.Terminal.Prelude
  ( module Control.Lens,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.State.Strict,
    module GHC.Records,
    module Data.Foldable,
    makeLenses',
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
makeLenses' = makeFieldOptics (lensRules {_fieldToDef = \_ _ n -> [TopName (mkName (nameBase n))]})
