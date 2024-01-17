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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : 'EventBackend' rendering 'Observe.Event.Event's as OpenTelemetry traces
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com
-}
module Observe.Event.Backend.OpenTelemetry
  ( TracerEventBackend (..)
  , RenderOTel
  , OTelRendered (..)
  )
where

import Control.Monad.IO.Class
import Data.Functor.Parametric
import Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text, pack)
import Observe.Event.Backend
import OpenTelemetry.Context as Context
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Trace.Core

{- | An 'EventBackend' built on a 'Tracer'.

When no explicit parent is set, the backend will try to find a parent in the "OpenTelemetry.Context.ThreadLocal" 'Context'.
However, it will never update that 'Context', as the primitive 'EventBackend' API has no way to determine if it's being
consumed in a scoped context or one allowing for general interleaving.

When possible, events created with 'Observe.Event.instantEvent' will use the span event API. However, this requires a parent event
(specified through @e11y@'s interfaces or found in the thread-local 'Context'), without which backend will fallback to creating
and 'endSpan'ing a new 'Span'. If a span event is created, the resulting 'Observe.Event.eventReference' will refer to its parent,
as span events cannot be parents/links. Span events do not allow for non-parent links, so any @causes@ are dropped; in the future,
we may either add them as custom 'Attribute's or fall back to a full span if any are specified.

Event t'Link's are currently not given any attributes. In the future, arbitrary link metadata could be added to the core 'EventBackend'
API, in which case we could add a renderer for the link metadata type.

The underlying 'Tracer' is responsible for timestamping.

Exceptions are 'recordException'ed as escaped exceptions, without any custom attributes.
In the future, an @Exception -> HashMap Text Attribute@ argument could be added, or
arbitrary exception metadata added to 'Observe.Event.finalizeEvent'.
-}
data TracerEventBackend selector = TracerEventBackend
  { tracer ∷ !Tracer
  -- ^ The 'Tracer' from @hs-opentelemetry-api@.
  --
  -- See the [hs-opentelemetry-sdk intialization docs](https://hackage.haskell.org/package/hs-opentelemetry-sdk#initialization) for
  -- the typical way of getting a 'Tracer' in your application.
  , render ∷ !(RenderOTel selector)
  -- ^ The domain-specific logic for [rendering](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:rendering)
  -- 'Observe.Event.Event's rooted in a given [selector](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:selectorAndField).
  }

{- | The domain-specific logic for [rendering](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:rendering)
'Observe.Event.Event's rooted in a given [selector](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:selectorAndField).
-}
type RenderOTel selector = ∀ field. Selectors selector field → OTelRendered field

{- | The domain-specific logic for [rendering](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:rendering)
a specific 'Observe.Event.Event' with a given [field](https://hackage.haskell.org/package/e11y/docs/Observe-Event.html#g:selectorAndField)
type.
-}
data OTelRendered field = OTelRendered
  { eventName ∷ !Text
  -- ^ The name of the event.
  --
  -- See the "span name" section of the [open-telemetry span documentation](https://opentelemetry.io/docs/reference/specification/trace/api/#span).
  , eventKind ∷ !SpanKind
  -- ^ The kind of span to create.
  --
  -- See the [SpanKind](https://opentelemetry.io/docs/reference/specification/trace/api/#spankind) specification.
  , renderField ∷ !(field → HashMap Text Attribute)
  -- ^ Render a field to a set of span [attributes](https://opentelemetry.io/docs/reference/specification/common/#attribute).
  --
  -- Note especially the [attribute naming guidelines](https://opentelemetry.io/docs/reference/specification/common/attribute-naming/).
  }

instance EventBackend (TracerEventBackend selector) where
  type BackendEvent (TracerEventBackend selector) = TracerEventBackendEvent
  type RootSelector (TracerEventBackend selector) = selector

instance (MonadIO m, ParametricFunctor m) ⇒ EventBackendIn m (TracerEventBackend selector) where
  newEvent be params = do
    ctx ← fromMaybe Context.empty <$> lookupContext
    let ctx' = maybe ctx (`insertSpan` ctx) params.parent
        rendered = render be params.selectors
    links ← traverse (fmap (`NewLink` HashMap.empty) . getSpanContext) params.causes
    otelSpan ←
      createSpanWithoutCallStack be.tracer ctx' rendered.eventName $
        SpanArguments
          { kind = rendered.eventKind
          , attributes = foldMap (renderField rendered) params.initialFields
          , links
          , startTime = Nothing
          }
    pure $ TracerEventBackendEvent{otelSpan, renderField' = renderField rendered}

  newInstantEvent be params = case params.parent of
    Nothing → do
      m_ctx ← lookupContext
      case m_ctx >>= lookupSpan of
        Just s → newInstantEvent be $ params{parent = Just s}
        Nothing → do
          ev ← newEvent be params
          finalize ev Nothing
          pure $ reference ev
    Just s → do
      let rendered = render be params.selectors
      addEvent s $
        NewEvent
          { newEventName = rendered.eventName
          , newEventAttributes = foldMap (renderField rendered) params.initialFields
          , newEventTimestamp = Nothing
          }
      pure s

data TracerEventBackendEvent field = TracerEventBackendEvent
  { renderField' ∷ !(field → HashMap Text Attribute)
  , otelSpan ∷ Span
  }

instance Observe.Event.Backend.Event TracerEventBackendEvent where
  type EventReference TracerEventBackendEvent = Span
  reference = (.otelSpan)

instance (MonadIO m, ParametricFunctor m) ⇒ EventIn m TracerEventBackendEvent where
  finalize ev err = do
    case err of
      Just e → do
        recordException ev.otelSpan (singleton "exception.escaped" (toAttribute True)) Nothing e
        setStatus ev.otelSpan . Error . pack $ show e
      Nothing → setStatus ev.otelSpan Ok
    endSpan ev.otelSpan Nothing
  addField ev = addAttributes ev.otelSpan . renderField' ev
