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
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Event-oriented instrumentation
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This is the primary module needed to instrument code with @e11y@, or to consume instrumentation.

Instrumentors should first define selector types
appropriate to the unit of code they're instrumenting:

#selector#
Selectors are values which designate the general category of event
being created, parameterized by the type of fields that can be added to it.
For example, a web service's selector type may have a @ServicingRequest@
constructor, whose field type includes a @ResponseCode@ constructor which
records the HTTP status code. Selectors are intended to be of a domain-specific
type per unit of functionality within an instrumented codebase, implemented as a GADT.

Instrumentation then centers around 'Event's. 'Event's are <#g:initialization initialized> in
appropriate computational contexts (see 'HasEvents') by passing an appropriate selector value.

Consumers of instrumentation implement instances of the 'EventBackend' class.
-}
module Observe.Event where

import Control.Monad.With
import Data.GeneralAllocate
import Data.Kind
import Data.Monoid

-- * Event initialization #initialization#

-- | Run a computation during an event selected by the [selector](#selector).
withEvent
  ∷ (HasEvents m backend selector, MonadWith m)
  ⇒ selector field
  -- ^ The event [selector](#selector)
  → ((HasEvent m (Event backend) field) ⇒ m a)
  -- ^ The eventful computation
  → m a
withEvent selector go = generalWith (allocateEvent selector) $ \ev → let ?e11yEvent = ev in go

-- | A t'GeneralAllocate'-ion of a new event, selected by the [selector](#selector)
allocateEvent
  ∷ (HasEvents m backend selector, Applicative m)
  ⇒ selector field
  -- ^ The event [selector](#selector)
  → GeneralAllocate m e () releaseArg (Event backend field)
allocateEvent sel = GeneralAllocate $ \_ → do
  ev ← newEvent ?e11yBackend sel
  pure $ GeneralAllocated ev (const $ getAp mempty)

{- | A computational context with an 'EventBackend' supporting a given [selector](#selector).

In typical usage, @backend@ will be kept as a type parameter, to be determined
at the call site by the dynamically-scoped @?e11yBackend@ parameter.
-}
type HasEvents m backend selector = (?e11yBackend ∷ backend, EventBackend m selector backend)

{- | A computational context occurring during an event of the given @field@ type.

In typical usage, @event@ will be kept as a type parameter, to be determined
at the call site by the dynamically-scoped @?e11yEvent@ parameter.
-}
type HasEvent m event field = (?e11yEvent ∷ event field)

-- * Consuming instrumentation

-- | A resource implementing events of a given [selector](#selector) type in a given @m@onad
class EventBackend m selector backend where
  -- | The type of events
  type Event backend ∷ Type → Type

  -- | Create a new event selected by the [selector](#selector)
  --
  -- Callers will typically want to use the [resource-safe event initialization functions](#initialization)
  -- instead of using this directly.
  newEvent
    ∷ backend
    -- ^ The event backend
    → selector field
    -- ^ The event [selector](#selector)
    → m (Event backend field)
