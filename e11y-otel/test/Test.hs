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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Exception
import Control.Monad.With
import Data.Foldable
import Data.HashMap.Strict hiding (toList)
import Data.IORef
import Data.Kind
import Data.Maybe
import Data.Text qualified as T
import Observe.Event
import Observe.Event.Backend
import Observe.Event.Backend.OpenTelemetry
import OpenTelemetry.Attributes
import OpenTelemetry.Context
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Exporter.InMemory
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core
import OpenTelemetry.Util
import Test.Syd

data TestSelector ∷ Type → Type where
  Test ∷ TestSelector TestField

data TestField = TestField deriving (Eq, Show)

type instance SubSelector TestField = SubTestSelector

data SubTestSelector ∷ Type → Type where
  SubTest ∷ SubTestSelector SubTestField

data SubTestField = SubTestField deriving (Eq, Show)

type instance SubSelector SubTestField = NoEventsSelector

renderTestSelector ∷ RenderOTel TestSelector
renderTestSelector = selectorRendering $ \Test →
  SelectorRendering
    { renderTopSelector =
        OTelRendered
          { eventName = "test"
          , eventKind = Internal
          , renderField = \TestField → singleton "test-field" (AttributeValue $ BoolAttribute True)
          }
    , renderSubSelector = renderSubTestSelector
    }

renderSubTestSelector ∷ RenderOTel SubTestSelector
renderSubTestSelector = selectorRendering $ \SubTest →
  noSubEventsSelectorRendering $
    OTelRendered
      { eventName = "sub-test"
      , eventKind = Internal
      , renderField = \SubTestField → singleton "sub-test-field" (AttributeValue $ BoolAttribute True)
      }

data TestException = TestException deriving (Show)

instance Exception TestException

dummyTracer ∷ IO (Tracer, IORef [ImmutableSpan])
dummyTracer = do
  (_, options) ← getTracerProviderInitializationOptions
  (processor, var) ← inMemoryListExporter
  provider ← createTracerProvider [processor] options
  pure
    ( makeTracer provider (InstrumentationLibrary "dummy" "1.0.0") tracerOptions
    , var
    )

main ∷ IO ()
main = sydTest $ do
  let setupTracer = do
        (tracer, spansVar) ← dummyTracer
        pure (spansVar, tracer)
  before setupTracer $ describe "TracerEventBackend" $ do
    it "creates spans" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      (withEvent Test $ pure ()) ∷ IO ()
      spans ← readIORef spansVar

      s ← case spans of
        [s] → pure s
        _ → expectationFailure "wrong number of spans"

      shouldSatisfyNamed
        s
        "has the expected name"
        (\s' → s'.spanName == "test")

    it "creates parentless events when the Context is empty" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      (withEvent Test $ pure ()) ∷ IO ()
      spans ← readIORef spansVar

      s ← case spans of
        [] → expectationFailure "no spans created"
        [s] → pure s
        _ → expectationFailure "too many spans created"

      shouldSatisfyNamed
        s
        "has no parent"
        (\s' → isNothing s'.spanParent)

    it "gets the parent from the Context when events are parentless" $ \(spansVar, tracer) → do
      let be = TracerEventBackend{tracer, render = renderTestSelector}
      let ?e11yBackend = be
      _ ∷ () ← withEvent Test $ do
        adjustContext $ insertSpan eventReference
        let ?e11yBackend = be
        (withEvent Test $ pure ()) ∷ IO ()
      spans ← readIORef spansVar

      (parent, child) ← case spans of
        [parent, child] → pure (parent, child)
        _ → expectationFailure "wrong number of spans"

      childParentContext ← traverse getSpanContext child.spanParent
      case childParentContext of
        Nothing → expectationFailure "child has no parent"
        Just ctx → shouldSatisfyNamed ctx "parent is the right parent" (== parent.spanContext)

    it "ignores the Context when an event has a parent" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      (s1 ∷ Span) ← withEvent Test $ pure eventReference
      _ ∷ () ← withEvent Test $ do
        adjustContext $ insertSpan s1
        (withEvent SubTest $ pure ()) ∷ IO ()
      spans ← readIORef spansVar

      (parent, child) ← case spans of
        [parent, child, _] → pure (parent, child)
        _ → expectationFailure "wrong number of spans"

      childParentContext ← traverse getSpanContext child.spanParent
      case childParentContext of
        Nothing → expectationFailure "child has no parent"
        Just ctx → shouldSatisfyNamed ctx "parent is the right parent" (== parent.spanContext)

    it "adds causes as links" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      _ ∷ () ← withEvent Test $ do
        withRelatedEvent SubTest Nothing [eventReference] $ do pure ()
      spans ← readIORef spansVar

      (parent, child) ← case spans of
        [parent, child] → pure (parent, child)
        _ → expectationFailure "wrong number of spans"

      childLink ← case toList $ frozenBoundedCollectionValues child.spanLinks of
        [childLink] → pure childLink
        _ → expectationFailure "wrong number of links"

      shouldSatisfyNamed
        childLink
        "links to parent"
        (\cl → cl.frozenLinkContext == parent.spanContext)

    it "adds initial fields" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      let alloc =
            allocateEventArgs $
              EventParams
                { selectors = Leaf Test
                , parent = Nothing
                , causes = []
                , initialFields = [TestField]
                }
      generalWith alloc $ \_ → pure ()
      spans ← readIORef spansVar

      s ← case spans of
        [s] → pure s
        _ → expectationFailure "wrong number of spans"

      let checkAttrs s' = (snd . getAttributes $ s'.spanAttributes) ! "test-field" == AttributeValue (BoolAttribute True)

      shouldSatisfyNamed
        s
        "has a test attr"
        checkAttrs

    it "creates instant parentless events as spans when the Context is empty" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      !_ ← instantEvent Test []
      spans ← readIORef spansVar

      s ← case spans of
        [] → expectationFailure "no spans created"
        [s] → pure s
        _ → expectationFailure "too many spans created"

      shouldSatisfyNamed
        s
        "has no parent"
        (\s' → isNothing s'.spanParent)

    it "creates instant parentless events as span events of the Context's span when it has one" $ \(spansVar, tracer) → do
      let be = TracerEventBackend{tracer, render = renderTestSelector}
      let ?e11yBackend = be
      _ ∷ () ← withEvent Test $ do
        adjustContext $ insertSpan eventReference
        let ?e11yBackend = be
        !_ ← instantEvent Test []
        pure ()
      spans ← readIORef spansVar

      parent ← case spans of
        [parent] → pure parent
        _ → expectationFailure "wrong number of spans"

      let evs = toList . appendOnlyBoundedCollectionValues $ parent.spanEvents
      child ← case evs of
        [child] → pure child
        _ → expectationFailure "wrong number of span events"
      shouldSatisfyNamed
        child
        "is a test event"
        (\c → c.eventName == "test")

    it "adds fields span events generated by parented instant events" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      _ ∷ () ← withEvent Test $ do
        _ ← instantEvent SubTest [SubTestField]
        pure ()
      spans ← readIORef spansVar

      parent ← case spans of
        [parent] → pure parent
        _ → expectationFailure "wrong number of spans"

      let evs = toList . appendOnlyBoundedCollectionValues $ parent.spanEvents
      child ← case evs of
        [child] → pure child
        _ → expectationFailure "wrong number of span events"

      let checkAttrs e = (snd . getAttributes $ e.eventAttributes) ! "sub-test-field" == AttributeValue (BoolAttribute True)

      shouldSatisfyNamed
        child
        "has a subtest attr"
        checkAttrs

    it "sets event exceptions" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      (withEvent Test $ finalizeEvent (Just (SomeException TestException))) ∷ IO ()
      spans ← readIORef spansVar

      s ← case spans of
        [s] → pure s
        _ → expectationFailure "wrong number of spans"

      let evs = toList . appendOnlyBoundedCollectionValues $ s.spanEvents
          checkAttrs e = (snd . getAttributes $ e.eventAttributes) ! "exception.escaped" == AttributeValue (BoolAttribute True)
      errorEv ← case evs of
        [errorEv] → pure errorEv
        _ → expectationFailure "wrong number of span events"

      shouldSatisfyNamed
        (s, errorEv)
        "has exception information"
        (\(s', errorEv') → s'.spanStatus == Error (T.pack $ show TestException) && checkAttrs errorEv')

    it "adds fields as attributes" $ \(spansVar, tracer) → do
      let ?e11yBackend = TracerEventBackend{tracer, render = renderTestSelector}
      (withEvent Test $ addEventField TestField) ∷ IO ()
      spans ← readIORef spansVar

      s ← case spans of
        [s] → pure s
        _ → expectationFailure "wrong number of spans"

      let checkAttrs s' = (snd . getAttributes $ s'.spanAttributes) ! "test-field" == AttributeValue (BoolAttribute True)

      shouldSatisfyNamed
        s
        "has a test attr"
        checkAttrs
