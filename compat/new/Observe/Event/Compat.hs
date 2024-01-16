-- Copyright 2024 Shea Levy
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Compatibility shims for old libraries
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com
-}
module Observe.Event.Compat where

import Data.Kind

{- | Work around lack of t'Control.Monad.Trans.Class.MonadTrans' monad-preserving superconstraint in
older @transformers@.
-}
type MonadTransMonadConstraint (t ∷ (Type → Type) → (Type → Type)) (m ∷ Type → Type) = () ∷ Constraint
