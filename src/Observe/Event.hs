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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

    -- ** Selector rendering

    -- | 'EventBackend's will typically require the user to provide a "rendering function"
    -- to translate 'Event'-based instrumentation into the format required for the backend.
    -- The full type of this will depend on the backend in question, but it will typically
    -- involve a function of the form @Selectors selector field -> T field@ for some @T@
    -- (perhaps a function type taking further arguments). The 'Selectors' type is not
    -- the simplest to work with, but in most cases the 'selectorRendering' function can
    -- be used to write your renderer.
    --
    -- Consider the following selector tree:
    --
    -- > module Parent where
    -- >
    -- > import Child
    -- >
    -- > data ParentSelector field where
    -- >   ParentA :: ParentSelector AField
    -- >   ParentB :: ParentSelector BField
    -- >
    -- > data AField = AField Int
    -- > type instance SubSelector AField = NoEventsSelector
    -- >
    -- > data BField = BYes | BNo
    -- > type instance SubSelector BField = ChildSelector
    --
    -- > module Child where
    -- >
    -- > data ChildSelector field where
    -- >   Child :: ChildSelector CField
    -- >
    -- > data CField = CField
    -- > type instance SubSelector CField = NoEventsSelector
    --
    -- You could write renderers for these as follows:
    --
    -- > module Parent where -- Or it could be in a separate module if you want to keep instrumentation and rendering apart
    -- >
    -- > renderParentSelector :: Selectors ParentSelector field -> Identity field -- Obviously in a real case it wouldn't be Identity
    -- > renderParentSelector = selectorRendering $ \case
    -- >   ParentA -> noSubEventsSelectorRendering (Identity $ AField 0)
    -- >   ParentB -> SelectorRendering
    -- >     { renderTopSelector = Identity BYes
    -- >     , renderSubSelector = renderChildSelector -- You could also modify the T CField after calling the child renderer
    -- >     }
    --
    -- > module Child where
    -- >
    -- > renderChildSelector :: Selectors ChildSelector field -> Identity field
    -- > renderChildSelector = selectorRendering $ \Child ->
    -- >   noSubEventsSelectorRendering $ Identity CField
  , selectorRendering
  , SelectorRendering (..)
  , noSubEventsSelectorRendering
  , Selectors (..)

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
eventReference ∷ (HasEvent event field) ⇒ EventReference event
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
  ∷ (HasEventIn m event field)
  ⇒ Maybe SomeException
  → m ()
finalizeEvent = finalize ?e11yEvent

{- | A computational context supporting creating 'Event's from a given [selector](#g:selectorAndField) family.

In typical usage, @backend@ will be kept as a type parameter, to be determined
at the call site by the dynamically-scoped @?e11yBackend@ parameter.

'HasEvents' can be satisfied by binding the @?e11yBackend@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value. t'Data.Proxy.Proxy' @selector@ can be used as a no-op 'EventBackend', and a pair of backends with the same 'RootSelector' can be used as a backend as well.
-}
type HasEvents m backend selector = (?e11yBackend ∷ backend, EventBackendIn m backend, selector ~ RootSelector backend)

{- | A scope containing an event of the given @field@ type.

In typical usage, @event@ will be kept as a type parameter,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ parameter.

'HasEvent' can be satisfied by binding the @?e11yEvent@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value. 'withEvent' handles this for you.
-}
type HasEvent event field = (?e11yEvent ∷ event field, Event event)

{- | A computational context occurring during an event of the given @field@ type.

In typical usage, @event@ will be kept as a type parameter,
to be determined at the call site by the dynamically-scoped @?e11yEvent@ parameter.

'HasEventIn' can be satisfied by binding the @?e11yEvent@ [implicit parameter](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to an appropriate value. 'withEvent' handles this for you.
-}
type HasEventIn m event field = (HasEvent event field, EventIn m event)

{- | A computational context occurring during an event of the given @field@ type, allowing
the creation of new 'Event's according to its 'SubSelector' which are children of the given
event.

In typical usage, @backend@ will be kept as a type parameter,
to be determined at the call site by the dynamically-scoped @?e11yBackend@ parameter.

'HasSubEvents' can be satisfied by binding the @?e11yEvent@ and @?e11yBackend@ [implicit parameters](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/implicit_parameters.html)
to appropriate values, the latter via v'SubEventBackend'. 'withEvent' handles this for you.
-}
type HasSubEvents m backend field = (HasEventIn m (BackendEvent backend) field, ?e11yBackend ∷ SubEventBackend backend field)

{- | A selector type with no values.

This results in an 'EventBackend' which cannot create
any 'Event's, which is useful to terminate the tree of
event types generated by 'SubSelector'
-}
type NoEventsSelector ∷ Type → Type
data NoEventsSelector f

-- | Generate a rendering function from a t'SelectorRendering' for each top-level selector.
selectorRendering
  ∷ (∀ field. selector field → SelectorRendering t field)
  → ∀ field
   . Selectors selector field
  → t field
selectorRendering getRendering (Leaf s) = (getRendering s).renderTopSelector
selectorRendering getRendering (s :/ tl) = renderSubSelector (getRendering s) tl

-- | Generate a t'SelectorRendering' for events with no sub-events.
noSubEventsSelectorRendering ∷ (SubSelector field ~ NoEventsSelector) ⇒ t field → SelectorRendering t field
noSubEventsSelectorRendering renderTopSelector =
  SelectorRendering
    { renderTopSelector
    , renderSubSelector = selectorRendering $ \case {}
    }

-- | Data needed to define a rendering of 'Selectors' of a given field type.
data SelectorRendering t field = SelectorRendering
  { renderTopSelector ∷ !(t field)
  -- ^ Handle the case where the top of the tree was selected
  , renderSubSelector ∷ !(∀ field'. Selectors (SubSelector field) field' → t field')
  -- ^ Handle a sub-event
  }

{- | An 'EventBackend' to use in the context of a running 'Event'.

It creates events selected by the 'SubSelector' of the event's @field@ type,
and any parentless events created by it are made children of the event.
-}
data SubEventBackend backend field = SubEventBackend
  { selector ∷ !(RootSelector backend field)
  -- ^ The selector of the @backend@ which 'Event's should be nested under.
  , backend ∷ !backend
  -- ^ The underlying @backend@
  , parentReference ∷ !(EventReference (BackendEvent backend))
  -- ^ A [reference](#g:relationships) to the running 'Event'
  }

newtype SubEventBackendEvent backend field = SubEventBackendEvent (BackendEvent backend field)

deriving newtype instance (EventBackend backend) ⇒ Event (SubEventBackendEvent backend)

deriving newtype instance (EventBackendIn m backend, Monad m) ⇒ EventIn m (SubEventBackendEvent backend)

{- | Create 'Event's in the parent 'EventBackend' which are children
of the running 'Event' and are selected by the 'SubSelector' of its
@field@ type.
-}
instance (EventBackend backend) ⇒ EventBackend (SubEventBackend backend field) where
  type BackendEvent (SubEventBackend backend field) = SubEventBackendEvent backend
  type RootSelector (SubEventBackend backend field) = SubSelector field

subEventParams ∷ SubEventBackend backend field → EventParams (SubSelector field) field' (EventReference (BackendEvent backend)) → EventParams (RootSelector backend) field' (EventReference (BackendEvent backend))
subEventParams backend params =
  params
    { selectors = backend.selector :/ params.selectors
    , parent = params.parent <|> Just backend.parentReference
    }

{- | Create 'Event's in the parent 'EventBackend' which are children
of the running 'Event' and are selected by the 'SubSelector' of its
@field@ type.
-}
instance (EventBackendIn m backend, ParametricFunctor m) ⇒ EventBackendIn m (SubEventBackend backend field) where
  newEvent ∷ ∀ field'. SubEventBackend backend field → EventParams (SubSelector field) field' (EventReference (BackendEvent backend)) → m (SubEventBackendEvent backend field')
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
  ∷ (HasEventIn m event field)
  ⇒ field
  → m ()
addEventField = addField ?e11yEvent
