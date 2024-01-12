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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Consuming events as data
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module provides the t'DataEventBackend' 'EventBackend' for consuming events
by representing them as ordinary Haskell data.
-}
module Observe.Event.Data (DataEventBackend, newDataEventBackend, getEvents, DataEvent (..)) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Primitive.MutVar
import Data.Sequence
import Observe.Event

{- | An 'EventBackend' for consuming events by representing them as
ordinary Haskell data.
-}
newtype DataEventBackend m selector = DataEventBackend (MutVar (PrimState m) (Seq (DataEvent selector)))

-- | Allocate a new t'DataEventBackend'
newDataEventBackend ∷ ∀ m selector. (PrimMonad m, ∀ x y. (Coercible x y) ⇒ Coercible (m x) (m y)) ⇒ m (DataEventBackend m selector)
newDataEventBackend = coerce $ newMutVar @m (empty @(DataEvent selector))

-- | Read the events that have been emitted using the t'DataEventBackend'
getEvents
  ∷ ∀ m selector
   . (PrimMonad m, ∀ x y. (Coercible x y) ⇒ Coercible (m x) (m y))
  ⇒ DataEventBackend m selector
  -- ^ The backend
  → m (Seq (DataEvent selector))
getEvents = coerce (readMutVar @m @(Seq (DataEvent selector)))

-- | A representation of an event.
data DataEvent selector = ∀ f.
  DataEvent
  { selector ∷ !(selector f)
  -- ^ The selector used to initialize the event
  }

-- | The 'Event' associated with t'DataEventBackend'.
data DataEventBackendEvent f = DataEventBackendEvent

type instance Event (DataEventBackend m selector) = DataEventBackendEvent

instance (PrimMonad m) ⇒ EventBackend m selector (DataEventBackend m selector) where
  newEvent eb s = do
    atomicModifyMutVar' (coerce eb) (\l → (l |> DataEvent s, ()))
    pure DataEventBackendEvent
