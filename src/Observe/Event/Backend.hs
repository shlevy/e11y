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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Description : Interface for implementing EventBackends
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This is the primary module needed to write a new 'EventBackend' and make it an 'EventBackendIn' relevant monads.
-}
module Observe.Event.Backend where

import Control.Exception
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Functor.Parametric
import Data.Kind

-- * Defining backends

{- | A resource allowing creation of new 'Event's.

It must be an 'EventBackendIn' some monad to be useful.
-}
class (Event (BackendEvent backend)) ⇒ EventBackend backend where
  -- | The 'Event' type this 'EventBackend' can generate.
  --
  -- 'Event's are parameterized by the type of [fields](Observe-Event.html#g:selectorAndField)
  -- they support.
  type BackendEvent backend = (event ∷ Type → Type) | event → backend

  -- | The root of the [selector tree](Observe-Event.html#g:selectorAndField) this 'EventBackend' supports.
  type RootSelector backend ∷ Type → Type

-- | An 'EventBackend' which can be used in a given @m@onad
class (EventBackend backend, EventIn m (BackendEvent backend)) ⇒ EventBackendIn m backend where
  -- | Create a new 'Event' with the given @field@ type.
  --
  -- Callers must ensure the resulting 'Event' is
  -- 'finalize'd; the higher-level [event initialization functions](Observe-Event.html#g:init)
  -- take care of this for you.
  newEvent
    ∷ backend
    → EventParams (RootSelector backend) field (EventReference (BackendEvent backend))
    -- ^ Specify the event, matching the appropriate [selector](Observe-Event.html#g:selectorAndField)
    -- type for this 'EventBackend'.
    → m (BackendEvent backend field)

-- | Parameters specifying a new 'Event'
data EventParams selector field reference = EventParams
  { selectors ∷ !(Selectors selector field)
  -- ^ [Select](Observe-Event.html#g:selectorAndField) the type of the new 'Event'
  , parent ∷ !(Maybe reference)
  -- ^ A [reference](Observe-Event.html#g:relationships) to the parent of the new 'Event', if it has one.
  , causes ∷ ![reference]
  -- ^ [References](Observe-Event.html#g:relationships) to the (proximate) causes of the new 'Event', if any.
  }

-- | A @DerivingVia@ helper for lifting an 'EventBackend' into a 'MonadTrans'formed monad.
newtype LiftBackend backend = LiftBackend backend

-- | Lift an 'EventBackend' into a 'MonadTrans'formed monad.
instance (EventBackend backend) ⇒ EventBackend (LiftBackend backend) where
  type BackendEvent (LiftBackend backend) = LiftBackendEvent backend
  type RootSelector (LiftBackend backend) = RootSelector backend

-- | Lift an 'EventBackend' into a 'MonadTrans'formed @m@onad.
instance (EventBackendIn m backend, MonadTrans t, ParametricFunctor m, ParametricFunctor (t m)) ⇒ EventBackendIn (t m) (LiftBackend backend) where
  newEvent ∷ ∀ field. LiftBackend backend → EventParams (RootSelector backend) field (EventReference (BackendEvent backend)) → t m (LiftBackendEvent backend field)
  newEvent = (lift .) . coerce (newEvent @m @backend @field)

-- ** Defining event types

{- | A resource allowing instrumentation of code via [fields](Observe-Event.html#g:selectorAndField) of
a given type.

It must be an 'EventIn' some monad to be useful.
-}
class Event event where
  -- | The type of [references](Observe-Event.html#g:relationships) to an 'Event'.
  type EventReference event ∷ Type

  -- | Get a [reference](Observe-Event.html#g:relationships) to this 'Event'
  reference ∷ event field → EventReference event

{- | An 'Event' which can be used in a given @m@onad.

Laws:

  * @finalize x >> finalize y@ = @finalize x@
-}
class (Event event, Monad m) ⇒ EventIn m event where
  -- | End the 'Event', perhaps due to an exception.
  --
  -- It is implementation-specific whether 'addField' after
  -- 'finalize' has any effect (but it is not an error).
  --
  -- Implementations should ensure that subsequent 'finalize'ations
  -- are no-ops.
  finalize ∷ event field → Maybe SomeException → m ()

  -- | Add a [field](Observe-Event.html#g:selectorAndField) to an 'Event'.
  addField ∷ event field → field → m ()

-- | The 'BackendEvent' type for 'LiftBackend'.
newtype LiftBackendEvent backend field = LiftBackendEvent (BackendEvent backend field)

deriving newtype instance (EventBackend backend) ⇒ Event (LiftBackendEvent backend)

-- | Lift an 'EventBackend' into a 'MonadTrans'formed @m@onad.
instance (EventBackendIn m backend, MonadTrans t, ParametricFunctor (t m)) ⇒ EventIn (t m) (LiftBackendEvent backend) where
  finalize ∷ ∀ field. LiftBackendEvent backend field → Maybe SomeException → t m ()
  finalize = (lift .) . coerce (finalize @m @(BackendEvent backend) @field)
  addField ∷ ∀ field. LiftBackendEvent backend field → field → t m ()
  addField = (lift .) . coerce (addField @m @(BackendEvent backend) @field)

-- * Selectors

{- | A nested sequence of selectors, starting from a
given root @selector@ family and ending in a selector
selecting a given @field@ type.

For example, given:

> data FooSelector :: Type -> Type where Foo :: FooSelector FooField
> data FooField
> type instance SubSelector FooField = BarSelector
> data BarSelector :: Type -> Type where Bar :: BarSelector BarField
> data BarField
> type instance SubSelector BarField = NoEventsSelector

Then @Leaf Foo@ is a sequence 'Selectors' picking out an 'Event' with field
type @FooField@, and @Foo :\/ Leaf Bar@ is a sequence of 'Selectors' picking out
an 'Event' with field type @BarField@ underneath an 'Event' with field type @FooField@.

See the [selector and field documentation](Observe-Event.html#g:selectorAndField) for more details.
-}
data Selectors selector field where
  Leaf ∷ selector field → Selectors selector field
  (:/) ∷ selector field → Selectors (SubSelector field) field' → Selectors selector field'

infixr 5 :/

{- | The [selector](Observe-Event.html#g:selectorAndField) type for sub-'Event's underneath
an 'Event' of the given @field@ type.

Instrumented code will typically involve nested scopes of events, including calls across
different modules, with different types of events expected in different contexts. To support
this, each field type has an associated 'SubSelector' type to identify the kind of events
that can occur in the scope where the event is active. This results in a tree of event
types represented by a tree of selectors, reflecting the tree of instrumented source
components: Each selector of the root selector type specifies a field type, which in turn
specifies yet another selector (and, with it, its sub-tree). Use cases which require picking
out a linear path through this tree can use 'Selectors'.

If there are no sub-'Event's under 'Event's with field type @f@, then you can use
 t'Observe.Event.NoEventsSelector': @type instance SubSelector f = NoEventsSelector@.

If you want to retain the set of possible event types as in the parent scope, simply set
'SubSelector' to the very same selector type that the parent selector came from.
-}
type family SubSelector field ∷ Type → Type
