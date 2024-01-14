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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  , instantEvent
  , Event

    -- ** Event relationships #relationships#

    -- | 'Event's can be related to each other in two ways: An 'Event' can have
    -- another 'Event' as a parent, and an 'Event' can have any number of other
    -- 'Event's as proximate causes.
    --
    -- In normal usage, these relationships are handled for you: 'withEvent'
    -- automatically marks new 'Event's created in the scope of the 'Event'
    -- as children.
    --
    -- If you need to specify more complex relationships, you can get a reference
    -- to an 'Event' using 'eventReference'. References are monad-independent data
    -- that can outlive the 'Event' itself, and are used to tell an 'EventBackend'
    -- which other 'Event's are involved in a new one.
  , eventReference
  , withRelatedEvent
  , allocateRelatedEvent
  , instantRelatedEvent

    -- ** 'Event'-supporting computational contexts #contexts#
  , HasEvents
  , HasEvent
  , HasEventIn
  , HasSubEvents

    -- ** Lower-level 'Event' allocation management
  , SubEventBackend (..)
  , allocateEventArgs
  , instantEventArgs

    -- * Event manipulation
  , addEventField
  , finalizeEvent
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.With
import Data.Coerce
import Data.Exceptable
import Data.Functor.Parametric
import Data.GeneralAllocate
import Data.Kind
import Observe.Event.Backend

-- * Event initialization #initialization#

{- | Run a computation during an 'Event' selected by the [selector](#g:selectorAndField).

Parentless 'Event's created during the computation will be marked as children of the
new 'Event'.

The 'Event' will be 'finalize'd at the end of the computation.

See 'withRelatedEvent' if you need to specify [relationships](#g:relationships).
-}
withEvent
  ∷ (HasEvents m backend selector, MonadWithExceptable m)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → ((HasSubEvents m backend field) ⇒ m a)
  -- ^ The eventful computation
  → m a
withEvent selector = withRelatedEvent selector Nothing []

{- | Emit an instantaneous 'Event' with the given [selector and fields](#g:selectorAndField).

See 'instantRelatedEvent' if you need to specify [relationships](#g:relationships).
-}
instantEvent
  ∷ (HasEvents m backend selector)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → [field]
  -- ^ The [fields](#g:selectorAndField) of the event.
  → m (EventReference (BackendEvent backend))
instantEvent selector fields = instantRelatedEvent selector fields Nothing []

{- | Emit an instantaneous 'Event' with the given [selector and fields](#g:selectorAndField)
and the given [parent and causes](#g:relationships).

See 'instantEvent' if you don't need to specify any relationships.

See 'instantEventArgs' for full control over 'Event' configuration.
-}
instantRelatedEvent
  ∷ (HasEvents m backend selector)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → [field]
  -- ^ The [fields](#g:selectorAndField) of the event.
  → Maybe (EventReference (BackendEvent backend))
  -- ^ The [parent](#g:relationships) of this event
  → [EventReference (BackendEvent backend)]
  -- ^ The [causes](#g:relationships) of this event
  → m (EventReference (BackendEvent backend))
instantRelatedEvent selector initialFields parent causes =
  instantEventArgs $
    EventParams
      { selectors = Leaf selector
      , parent
      , causes
      , initialFields
      }

{- | Emit an instantaneous 'Event' described by t'EventParams'

You probably want 'instantEvent' or 'instantRelatedEvent'
-}
instantEventArgs
  ∷ (HasEvents m backend selector)
  ⇒ EventParams selector field (EventReference (BackendEvent backend))
  → m (EventReference (BackendEvent backend))
instantEventArgs = newInstantEvent ?e11yBackend

{- | Run a computation during an 'Event' selected by the [selector](#g:selectorAndField) and with the given [parent and causes](#g:relationships).

Parentless 'Event's created during the computation will be marked as children of the
new 'Event'.

The 'Event' will be 'finalize'd at the end of the computation.

See 'withEvent' if you don't need to specify any relationships.

For a more flexible allocation, see 'allocateRelatedEvent'.
-}
withRelatedEvent
  ∷ (HasEvents m backend selector, MonadWithExceptable m)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → Maybe (EventReference (BackendEvent backend))
  -- ^ The [parent](#g:relationships) of this event
  → [EventReference (BackendEvent backend)]
  -- ^ The [causes](#g:relationships) of this event
  → ((HasSubEvents m backend field) ⇒ m a)
  -- ^ The eventful computation
  → m a
withRelatedEvent selector parent causes go = generalWith (allocateRelatedEvent selector parent causes) $
  \ev →
    let
      ?e11yEvent = ev
      ?e11yBackend = SubEventBackend selector ?e11yBackend $ reference ev
     in
      go

{- | A t'GeneralAllocate'-ion of a new 'Event', selected by the [selector](#g:selectorAndField)
and with the given [parent and causes](#g:relationships).

The 'Event' with be 'finalize'd upon release.

See 'allocateEventArgs' for full flexibility in specifying allocation.

You will likely want to construct a t'SubEventBackend' to create a
'HasSubEvents' context when using this 'Event'.
-}
allocateRelatedEvent
  ∷ (HasEvents m backend selector, Exceptable e)
  ⇒ selector field
  -- ^ The event [selector](#g:selectorAndField)
  → Maybe (EventReference (BackendEvent backend))
  -- ^ The [parent](#g:relationships) of this event
  → [EventReference (BackendEvent backend)]
  -- ^ The [causes](#g:relationships) of this event
  → GeneralAllocate m e () releaseArg (BackendEvent backend field)
allocateRelatedEvent selector parent causes =
  allocateEventArgs $
    EventParams
      { selectors = Leaf selector
      , causes
      , parent
      , initialFields = []
      }

-- | Get a [reference](#g:relationships) to the current 'Event'.
eventReference ∷ (HasEvent backend field) ⇒ EventReference (BackendEvent backend)
eventReference = reference ?e11yEvent

{- | A t'GeneralAllocate'-ion of a new 'Event' described by t'EventParams'

The 'Event' with be 'finalize'd upon release.

You probably want 'allocateRelatedEvent'.

You will likely want to construct a t'SubEventBackend' to construct a 'HasSubEvents'
context when using this 'Event'.
-}
allocateEventArgs
  ∷ (HasEvents m backend selector, Exceptable e)
  ⇒ EventParams selector field (EventReference (BackendEvent backend))
  -- ^ Specify the event, matching the appropriate [selector](Observe-Event.html#g:selectorAndField)
  -- type for this 'EventBackend'.
  → GeneralAllocate m e () releaseArg (BackendEvent backend field)
allocateEventArgs params = GeneralAllocate $ \unmask → do
  ev ← unmask $ newEvent ?e11yBackend params
  let release (ReleaseFailure e) = finalize ev . Just $ toSomeException e
      release (ReleaseSuccess _) = finalize ev Nothing
  pure $ GeneralAllocated ev release

{- | End the running 'Event' manually, perhaps due to an exception.

It is implementation-specific whether 'addEventField' after 'finalizeEvent'
has any effect (but it is not an error).

Subsequent 'finalize'ations, including those that result from leaving the
'withEvent' scope or releasing the 'allocateEventArgs' allocation, will be
no-ops.
-}
finalizeEvent
  ∷ (HasEventIn m backend field)
  ⇒ Maybe SomeException
  → m ()
finalizeEvent = finalize ?e11yEvent

{- | A computational context supporting creating 'Event's from a given [selector](#g:selectorAndField) family.

In typical usage, @backend@ will be kept as a type parameter, to be determined
at the call site by the dynamically-scoped @?e11yBackend@ parameter.

'HasEvents' can be satisfied by binding the @?e11yBackend@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value.
-}
type HasEvents m backend selector = (?e11yBackend ∷ backend, EventBackendIn m backend, selector ~ RootSelector backend)

{- | A scope containing an event of the given @field@ type.

In typical usage, @event@ and @backend@ will be kept as a type parameters,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ parameter.

'HasEvent' can be satisfied by binding the @?e11yEvent@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value. 'withEvent' handles this for you.
-}
type HasEvent backend field = (?e11yEvent ∷ BackendEvent backend field, EventBackend backend)

{- | A computational context occurring during an event of the given @field@ type.

In typical usage, @event@ and @backend@ will be kept as a type parameters,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ parameter.

'HasEventIn' can be satisfied by binding the @?e11yEvent@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value. 'withEvent' handles this for you.
-}
type HasEventIn m backend field = (HasEvent backend field, EventBackendIn m backend)

{- | A computational context occurring during an event of the given @field@ type, allowing
the creation of new 'Event's according to its 'SubSelector' which are children of the given
event.

In typical usage, @event@ and @backend@ will be kept as a type parameters,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ parameter.

'HasSubEvents' can be satisfied by binding the @?e11yEvent@ and @?e11yBackend@ [implicit parameters](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to appropriate values, the latter via v'SubEventBackend'. 'withEvent' handles this for you.
-}
type HasSubEvents m backend field = (HasEventIn m backend field, ?e11yBackend ∷ SubEventBackend backend field)

{- | A selector type with no values.

This results in an 'EventBackend' which cannot create
any 'Event's, which is useful to terminate the tree of
event types generated by 'SubSelector'
-}
type NoEventsSelector ∷ Type → Type
data NoEventsSelector f

{- | An 'EventBackend' to use in the context of a running 'Event'.

It creates events selected by the 'SubSelector' of the event's @field@ type,
and any parentless events created by it are made children of the event.
-}
data SubEventBackend backend field = SubEventBackend
  { selector ∷ !(RootSelector backend field)
  -- ^ The selector of the @backend@ which 'Event's should be nested under.
  , backend ∷ !backend
  -- ^ The underlying @backend@
  , reference ∷ !(EventReference (BackendEvent backend))
  -- ^ A [reference](#g:relationships) to the running 'Event'
  }

newtype SubEventBackendEvent backend field field' = SubEventBackendEvent (BackendEvent backend field')

deriving newtype instance (EventBackend backend) ⇒ Event (SubEventBackendEvent backend field)

deriving newtype instance (EventBackendIn m backend, Monad m) ⇒ EventIn m (SubEventBackendEvent backend field)

{- | Create 'Event's in the parent 'EventBackend' which are children
of the running 'Event' and are selected by the 'SubSelector' of its
@field@ type.
-}
instance (EventBackend backend) ⇒ EventBackend (SubEventBackend backend field) where
  type BackendEvent (SubEventBackend backend field) = SubEventBackendEvent backend field
  type RootSelector (SubEventBackend backend field) = SubSelector field

subEventParams ∷ SubEventBackend backend field → EventParams (SubSelector field) field' (EventReference (BackendEvent backend)) → EventParams (RootSelector backend) field' (EventReference (BackendEvent backend))
subEventParams backend params =
  params
    { selectors = backend.selector :/ params.selectors
    , parent = params.parent <|> Just backend.reference
    }

{- | Create 'Event's in the parent 'EventBackend' which are children
of the running 'Event' and are selected by the 'SubSelector' of its
@field@ type.
-}
instance (EventBackendIn m backend, ParametricFunctor m) ⇒ EventBackendIn m (SubEventBackend backend field) where
  newEvent ∷ ∀ field'. SubEventBackend backend field → EventParams (SubSelector field) field' (EventReference (BackendEvent backend)) → m (SubEventBackendEvent backend field field')
  newEvent backend params =
    coerce $
      newEvent @m @backend @field'
        backend.backend
        (subEventParams backend params)
  newInstantEvent ∷ ∀ field'. SubEventBackend backend field → EventParams (SubSelector field) field' (EventReference (BackendEvent backend)) → m (EventReference (BackendEvent backend))
  newInstantEvent backend params =
    coerce $
      newInstantEvent @m @backend @field'
        backend.backend
        (subEventParams backend params)

-- | Add a [field](Observe-Event.html#g:selectorAndField) to the running 'Event'.
addEventField
  ∷ (HasEventIn m backend field)
  ⇒ field
  → m ()
addEventField = addField ?e11yEvent
