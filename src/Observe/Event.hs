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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Event-oriented instrumentation
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This is the primary module needed to instrument code with @e11y@.

To /consume/ instrumentation, see "Observe.Event.Backend".
-}
module Observe.Event
  ( -- * Selectors and fields #selectorAndField#

    -- | Instrumentors should first define selector and field types appropriate
    -- to the unit of code they're instrumenting:
    --
    -- Selectors are values which designate the general category of event
    -- being [created](#g:init), parameterized by the type of fields that can be added to it.
    -- For example, a web service's selector type may have a @ServicingRequest@
    -- constructor, whose field type includes a @ResponseCode@ constructor which
    -- records the HTTP status code. Selectors are intended to be of a domain-specific
    -- type per unit of functionality within an instrumented codebase, implemented as a GADT.
    --
    -- Fields make up the basic data captured in an event. They should be added
    -- to an 'Event' (with 'addEventField') as the code progresses through various
    -- phases of work, and can be both milestone markers ("we got this far in the process")
    -- or more detailed instrumentation ("we've processed N records"). They are intended to
    -- be of a domain-specific type per unit of functionality within an instrumented codebase.
    SubSelector
  , NoEventsSelector

    -- * Event initialization #init#

    -- | Actual instrumentation centers around t'Event's, which can
    -- be initialized in the appropriate [computational contexts](#g:contexts)
    -- given an appropriate [selector](#g:selectorAndField) value.
  , withEvent
  , Event
  , allocateEvent

    -- ** 'Event'-supporting computational contexts #contexts#
  , HasEvents
  , HasEvent

    -- ** Lower-level 'Event' allocation management
  , SubEventBackend
  , finalizeEvent
  , allocateEvent'

    -- * Event manipulation

    -- | Once an t'Event' has been [initialized](#g:init), you can add
    -- [fields](Observe-Event.html#g:selectorAndField) to it (in the appropriate [computational contexts](#g:contexts))
  , addEventField
  )
where

import Control.Exception
import Control.Monad.With
import Data.Exceptable
import Data.GeneralAllocate
import Data.Kind
import Observe.Event.Backend

-- * Event initialization #initialization#

{- | Run a computation during an 'Event' selected by the [selector](#g:selectorAndField).

The 'Event' will be 'finalize'd at the end of the computation.
-}
withEvent
  ∷ (HasEvents m backend selector, MonadWithExceptable m)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → ((HasEvent m backend field) ⇒ m a)
  -- ^ The eventful computation
  → m a
withEvent selector go = generalWith (allocateEvent selector) $ \ev → let ?e11yEvent = ev; ?e11yBackend = SubEventBackend selector ?e11yBackend in go

{- | A t'GeneralAllocate'-ion of a new 'Event', selected by the [selector](#g:selectorAndField).

The 'Event' with be 'finalize'd upon release.

You will likely want to construct a t'SubEventBackend' to construct a 'HasEvent' context
when using this 'Event'.
-}
allocateEvent
  ∷ (HasEvents m backend selector, Exceptable e)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → GeneralAllocate m e () releaseArg (Event backend field)
allocateEvent = allocateEvent' . Leaf

{- | A t'GeneralAllocate'-ion of a new 'Event', selected by the sequence of 'Selectors'.

The 'Event' with be 'finalize'd upon release.

You probably want 'allocateEvent'.

You will likely want to construct a t'SubEventBackend' to construct a 'HasEvent' context
when using this 'Event'.
-}
allocateEvent'
  ∷ ∀ m backend selector field e releaseArg
   . (HasEvents m backend selector, Exceptable e)
  ⇒ Selectors selector field
  -- ^ A sequence of [selectors](#g:selectorAndField) identifying the t'Event's type.
  → GeneralAllocate m e () releaseArg (Event backend field)
allocateEvent' selectors = GeneralAllocate $ \_ → do
  ev ← newEvent ?e11yBackend selectors
  let release (ReleaseFailure e) = finalize @_ @backend ev . Just $ toSomeException e
      release (ReleaseSuccess _) = finalize @_ @backend ev Nothing
  pure $ GeneralAllocated ev release

{- | End an 'Event' manually, perhaps due to an exception.

Programs should not call 'addEventField' after 'finalizeEvent'.

Subsequent 'finalize'ations, including those that result from leaving the
'withEvent' scope or releasing the 'allocateEvent' allocation, will be
no-ops.
-}
finalizeEvent
  ∷ ∀ m backend field
   . (HasEvent m backend field)
  ⇒ Maybe SomeException
  → m ()
finalizeEvent = finalize @_ @backend ?e11yEvent

{- | A computational context supporting creating 'Event's from a given [selector](#g:selectorAndField) family.

In typical usage, @backend@ will be kept as a type parameter, to be determined
at the call site by the dynamically-scoped @?e11yBackend@ parameter.
-}
type HasEvents m backend selector = (?e11yBackend ∷ backend, EventBackend m backend, selector ~ RootSelector backend)

{- | A computational context occurring during an event of the given @field@ type.

In typical usage, @event@ and @backend@ will be kept as a type parameters,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ and
@?e11yBackend@ parameters.
-}
type HasEvent m backend field = (?e11yEvent ∷ Event backend field, ?e11yBackend ∷ SubEventBackend backend field, EventBackend m backend)

{- | A selector type with no values.

This results in an 'EventBackend' which cannot create
any 'Event's, which is useful to terminate the tree of
event types generated by 'SubSelector'
-}
type NoEventsSelector ∷ Type → Type
data NoEventsSelector f

{- | An 'EventBackend' supporting events within a given 'EventBackend' selected by the 'SubSelector'
of the given @field@ type.
-}
data SubEventBackend backend field = SubEventBackend
  { selector ∷ !(RootSelector backend field)
  -- ^ The selector of the @backend@ which 'Event's should be nested under.
  , backend ∷ !backend
  -- ^ The underlying @backend@
  }

-- | Use the same 'Event' type as the underlying 'EventBackend'
type instance Event (SubEventBackend backend field) = Event backend

-- | t'SubEventBackend's are rooted in the 'SubSelector' of their relevant fields
type instance RootSelector (SubEventBackend backend field) = SubSelector field

{- | Select 'Event's from the parent 'EventBackend' by prepending the selector
which yielded this 'Event' to the 'Selectors' given to the t'SubEventBackend'.
-}
instance (EventBackend m backend) ⇒ EventBackend m (SubEventBackend backend field) where
  newEvent ev selectors = newEvent ev.backend (ev.selector :/ selectors)
  finalize = finalize @_ @backend
  addField = addField @_ @backend

-- | Add a [field](Observe-Event.html#g:selectorAndField) to an 'Event'.
addEventField
  ∷ ∀ m backend field
   . (HasEvent m backend field)
  ⇒ field
  → m ()
addEventField = addField @_ @backend ?e11yEvent
