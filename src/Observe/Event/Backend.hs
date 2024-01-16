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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
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
module Observe.Event.Backend
  ( -- * Defining backends
    EventBackend (..)
  , EventBackendIn (..)
  , EventParams (..)

    -- ** Defining event types
  , Event (..)
  , EventIn (..)

    -- * Selectors
  , Selectors (..)
  , SubSelector
  )
where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Class.Parametric
import Control.Monad.Zip
import Data.Functor.Const
import Data.Functor.Parametric
import Data.Functor.Product
import Data.Kind
import Data.Proxy

{- | A resource allowing creation of new 'Event's.

It must be an 'EventBackendIn' some monad to be useful.
-}
class (Event (BackendEvent backend)) ⇒ EventBackend (backend ∷ Type) where
  -- | The 'Event' type this 'EventBackend' can generate.
  --
  -- 'Event's are parameterized by the type of [fields](Observe-Event.html#g:selectorAndField)
  -- they support.
  type BackendEvent backend ∷ Type → Type

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

  -- | Create an event which has no duration.
  --
  -- Returns a reference to the event.
  newInstantEvent
    ∷ backend
    → EventParams (RootSelector backend) field (EventReference (BackendEvent backend))
    → m (EventReference (BackendEvent backend))

-- | Parameters specifying a new 'Event'
data EventParams selector field reference = EventParams
  { selectors ∷ !(Selectors selector field)
  -- ^ [Select](Observe-Event.html#g:selectorAndField) the type of the new 'Event'
  , parent ∷ !(Maybe reference)
  -- ^ A [reference](Observe-Event.html#g:relationships) to the parent of the new 'Event', if it has one.
  , causes ∷ ![reference]
  -- ^ [References](Observe-Event.html#g:relationships) to the (proximate) causes of the new 'Event', if any.
  , initialFields ∷ ![field]
  -- ^ Fields to add to the 'Event' upon initialization.
  --
  -- This is especially useful in conjunction with 'newInstantEvent'
  }

-- | An 'EventBackend' that does nothing.
instance EventBackend (Proxy (selector ∷ Type → Type)) where
  type BackendEvent (Proxy selector) = Const ()
  type RootSelector (Proxy selector) = selector

-- | An 'EventBackend' that does nothing.
instance (Monad m, ParametricFunctor m) ⇒ EventBackendIn m (Proxy (selector ∷ Type → Type)) where
  newEvent _ _ = pure $ Const ()
  newInstantEvent _ _ = pure ()

{- | Combine two 'EventBackend's.

All operations are performed sequentially, with no
exception safety between calls.
-}
instance (EventBackend b1, EventBackend b2, RootSelector b1 ~ RootSelector b2) ⇒ EventBackend (b1, b2) where
  type BackendEvent (b1, b2) = Product (BackendEvent b1) (BackendEvent b2)
  type RootSelector (b1, b2) = RootSelector b1

{- | Combine two 'EventBackend's.

All operations are performed sequentially, with no
exception safety between calls.
-}
instance (EventBackendIn m b1, EventBackendIn m b2, RootSelector b1 ~ RootSelector b2) ⇒ EventBackendIn m (b1, b2) where
  newEvent (b1, b2) args = Pair <$> newEvent b1 args1 <*> newEvent b2 args2
   where
    (args1, args2) = unwrapPairParams args
  newInstantEvent (b1, b2) args = (,) <$> newInstantEvent b1 args1 <*> newInstantEvent b2 args2
   where
    (args1, args2) = unwrapPairParams args

{- | Lift an 'EventBackend' into a 'MonadTrans'formed @m@onad.

Note that this instance is [incoherent](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#overlapping-instances),
so it can be overridden for your @backend@ if need be. This instance will still be used in monad-generic code, however.
-}
instance {-# INCOHERENT #-} (EventBackendIn m backend, ParametricMonadTrans t) ⇒ EventBackendIn (t m) backend where
  newEvent = (lift .) . newEvent
  newInstantEvent = (lift .) . newInstantEvent

{- | A resource allowing instrumentation of code via [fields](Observe-Event.html#g:selectorAndField) of
a given type.

It must be an 'EventIn' some monad to be useful.
-}
class Event (event ∷ Type → Type) where
  -- | The type of [references](Observe-Event.html#g:relationships) to an 'Event'.
  type EventReference event ∷ Type

  -- | Get a [reference](Observe-Event.html#g:relationships) to this 'Event'
  reference ∷ event field → EventReference event

{- | An 'Event' which can be used in a given @m@onad.

Laws:

  * @finalize x >> finalize y@ = @finalize x@
-}
class (Event event, Monad m, ParametricFunctor m) ⇒ EventIn m event where
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

-- | An 'EventBackend' that does nothing.
instance Event (Const ()) where
  type EventReference (Const ()) = ()
  reference _ = ()

-- | An 'EventBackend' that does nothing.
instance (Monad m, ParametricFunctor m) ⇒ EventIn m (Const ()) where
  finalize _ _ = pure ()
  addField _ _ = pure ()

{- | Combine two 'EventBackend's.

All operations are performed sequentially, with no
exception safety between calls.
-}
instance (Event e1, Event e2) ⇒ Event (Product e1 e2) where
  type EventReference (Product e1 e2) = (EventReference e1, EventReference e2)
  reference (Pair e1 e2) = (reference e1, reference e2)

{- | Combine two 'EventBackend's.

All operations are performed sequentially, with no
exception safety between calls.
-}
instance (EventIn m e1, EventIn m e2) ⇒ EventIn m (Product e1 e2) where
  finalize (Pair e1 e2) err = finalize e1 err >> finalize e2 err
  addField (Pair e1 e2) f = addField e1 f >> addField e2 f

unwrapPairParams ∷ EventParams selector field (r1, r2) → (EventParams selector field r1, EventParams selector field r2)
unwrapPairParams params =
  ( params{parent = parent1, causes = causes1}
  , params{parent = parent2, causes = causes2}
  )
 where
  (parent1, parent2) = munzip params.parent
  (causes1, causes2) = munzip params.causes

{- | Lift an 'Event' into a 'MonadTrans'formed @m@onad.

Note that this instance is [incoherent](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#overlapping-instances),
so it can be overridden for your @event@ type if need be. This instance will still be used in monad-generic code, however.
-}
instance {-# INCOHERENT #-} (EventIn m event, ParametricMonadTrans t) ⇒ EventIn (t m) event where
  finalize = (lift .) . finalize
  addField = (lift .) . addField

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
