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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Description : Interface for implementing EventBackends
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This is the primary module needed to write new 'EventBackend's.
-}
module Observe.Event.Backend where

import Control.Exception
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Kind

-- * Defining backends

{- | A resource which can create t'Event's in a given @m@onad

Laws:

  * @finalize x >> finalize y@ = @finalize x@
-}
class (Monad m) ⇒ EventBackend m backend where
  -- | Create a new t'Event'.
  --
  -- Callers must ensure the resulting 'Event' is
  -- 'finalize'd; the higher-level [event initialization functions](Observe-Event.html#g:init)
  -- take care of this for you.
  newEvent
    ∷ backend
    → Selectors (RootSelector backend) field
    -- ^ Select the t'Event's type
    → m (Event backend field)

  -- | End the 'Event', perhaps due to an exception.
  --
  -- Callers should not call 'addField' after 'finalize'ation.
  --
  -- Implementations should ensure that subsequent 'finalize'ations
  -- are no-ops.
  finalize ∷ Event backend field → Maybe SomeException → m ()

  -- | Add a [field](Observe-Event.html#g:selectorAndField) to an 'Event'.
  addField ∷ Event backend field → field → m ()

{- | An event in a given 'EventBackend'.

'Event's are parameterized by the type of fields
they support. See the
[selector and field documentation](Observe-Event.html#g:selectorAndField) for more details.
-}
type family Event backend ∷ Type → Type

{- | The root of the selector tree this 'EventBackend' supports.

See the [selector and field documentation](Observe-Event.html#g:selectorAndField) for more details.
-}
type family RootSelector backend ∷ Type → Type

-- | A @DerivingVia@ helper for lifting 'EventBackend' instances through 'MonadTrans'
newtype LiftBackend backend = LiftBackend backend

-- | Keep the same 'Event' type when lifting through 'MonadTrans'
type instance Event (LiftBackend backend) = Event backend

-- | Keep the same root selector when lifting through 'MonadTrans'
type instance RootSelector (LiftBackend backend) = RootSelector backend

-- | Lift the underlying 'EventBackend' instance through 'MonadTrans'
instance (EventBackend m backend, MonadTrans t) ⇒ EventBackend (t m) (LiftBackend backend) where
  newEvent ∷ ∀ field. LiftBackend backend → Selectors (RootSelector backend) field → t m (Event backend field)
  newEvent = (lift .) . coerce (newEvent @m @backend @field)
  finalize ∷ ∀ field. Event backend field → Maybe SomeException → t m ()
  finalize = (lift .) . coerce (finalize @m @backend @field)
  addField ∷ ∀ field. Event backend field → field → t m ()
  addField = (lift .) . coerce (addField @m @backend @field)

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

{- | The [selector](Observe-Event.html#g:selectorAndField) type for sub-'Event's underneath
an 'Event' of the given @field@ type.

Instrumented code will typically involve nested scopes of events, including calls across
different modules, with different types of events expected in different contexts. To support
this, each field type has an associated 'SubSelector' type to identify the kind of events
that can occur in the scope where the event is active. This results in a tree of event
types represented by a tree of selectors, reflecting the tree of instrumented source
components: Each selector of the root selector type specifies a field type, which in turn
specifies yet another selector (and, with it, its sub-tree).

If there are no sub-'Event's under 'Event's with field type @f@, then you can use
t'Observe.Event.NoEventsSelector': @type instance SubSelector f = NoEventsSelector@.

If you want to retain the set of possible event types as in the parent scope, simply set
'SubSelector' to the very same selector type that the parent selector came from.
-}
type family SubSelector field ∷ Type → Type
