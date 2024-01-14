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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Exception
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.Catch.Pure qualified as Catch
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Either
import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Sequence
import Observe.Event
import Observe.Event.Backend
import Observe.Event.Data
import Test.Syd

data TestSelector ∷ Type → Type where
  Test ∷ TestSelector TestField

data TestField = TestField deriving (Eq, Show)

type instance SubSelector TestField = SubTestSelector

data SubTestSelector ∷ Type → Type where
  SubTest ∷ SubTestSelector SubTestField

data SubTestField = SubTestField deriving (Eq, Show)

type instance SubSelector SubTestField = NoEventsSelector

newtype EqSomeException = EqSomeException SomeException deriving newtype (Show)

instance Eq EqSomeException where
  EqSomeException e1 == EqSomeException e2 = show e1 == show e2

data DataEventTestSelectorFields
  = DataEventTestSelectorTestFields [TestField]
  | DataEventTestSelectorSubTestFields [SubTestField]
  deriving (Eq, Show)

data DataEventTestSelectorSelector
  = DataEventTestSelectorTest
  | DataEventTestSelectorTestSubTest
  deriving (Eq, Show)

data DataEventTestSelector = DataEventTestSelector
  { idx ∷ !Int
  , selector ∷ !DataEventTestSelectorSelector
  , parent ∷ !(Maybe (Either Int DataEventTestSelector))
  , causes ∷ ![Either Int DataEventTestSelector]
  , err ∷ !(Maybe EqSomeException)
  , fields ∷ !DataEventTestSelectorFields
  , instant ∷ !Bool
  }
  deriving (Eq, Show)

convDataEventTestSelector ∷ DataEvent TestSelector → DataEventTestSelector
convDataEventTestSelector ev@(DataEvent _ selectors _ _ _ fields _) =
  DataEventTestSelector
    { idx = ev.idx
    , parent = fmap convDataEventTestSelector <$> ev.parent
    , causes = fmap convDataEventTestSelector <$> ev.causes
    , err = coerce ev.err
    , fields = fields'
    , selector
    , instant = ev.instant
    }
 where
  (fields', selector) = case selectors of
    Leaf Test → (DataEventTestSelectorTestFields (toList fields), DataEventTestSelectorTest)
    Test :/ Leaf SubTest → (DataEventTestSelectorSubTestFields (toList fields), DataEventTestSelectorTestSubTest)
    Test :/ SubTest :/ Leaf impossible → case impossible of {}
    Test :/ SubTest :/ impossible :/ _ → case impossible of {}

newtype TestException = TestException Int deriving (Show)

instance Exception TestException

main ∷ IO ()
main = sydTest $ do
  describe "withEvent" $ do
    it "creates the event" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          idx ← withEvent Test $ do
            pure eventReference
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        shouldSatisfyNamed m_ev "was finalized and selected with the Test selector" (maybe False (\ev → ev.selector == DataEventTestSelectorTest))

    it "sets the backend to accept sub-selectors in the inner scope" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          idx ← withEvent Test $ do
            withEvent SubTest $ do
              addEventField SubTestField
              pure eventReference
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        shouldSatisfyNamed m_ev "was finalized and selected with the SubTest selector" (maybe False (\ev → ev.selector == DataEventTestSelectorTestSubTest))

    it "sets the backend to create child events" $
      let
        m_evs = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          (parentIdx, childIdx) ← withEvent Test $ do
            let parentIdx = eventReference
            withEvent SubTest $ do
              pure (parentIdx, eventReference)
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure (index evs parentIdx, index evs childIdx)
       in
        case m_evs of
          (Just parent, Just child) →
            shouldSatisfyNamed
              child
              "is a child of the parent"
              (\child' → child'.parent == Just (Right parent))
          (Nothing, _) → expectationFailure "Parent event was not finalized"
          (_, Nothing) → expectationFailure "Child event was not finalized"

    it "records and propagates exceptions" $
      let
        m_m_ev = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          e_res ← runCatchT $ withEvent Test $ do
            Catch.throwM $ TestException eventReference
          case e_res of
            Left e → case fromException e of
              Just (TestException idx) → do
                evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
                pure . Just $ index evs idx
              Nothing → pure Nothing
            _ → pure Nothing
       in
        case m_m_ev of
          Nothing → expectationFailure "TestException not thrown"
          Just m_ev →
            shouldSatisfyNamed
              m_ev
              "was finalized with a TestException"
              (maybe False (\ev → ev.err == (Just . EqSomeException . SomeException $ TestException ev.idx)))

  describe "LiftBackend" $
    it "lifts the EventBackend instance through a transformer" $
      runST $ do
        be ← newDataEventBackend @_ @TestSelector
        let
          ?e11yBackend = be
        e_res ← runCatchT $ withEvent Test $ do
          addEventField TestField
          subRef ← instantEvent SubTest []
          evs ← (fmap convDataEventTestSelector <$>) <$> lift (getEvents be)
          pure $
            reference (LiftBackendEvent ?e11yEvent) == 0
              && ( case index evs subRef of
                    Nothing → False
                    Just ev → ev.fields == DataEventTestSelectorSubTestFields []
                 )

        pure $ fromRight False e_res

  describe "finalizeEvent" $
    it "finalizes the event" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          idx ← withEvent Test $ do
            finalizeEvent $ Just (SomeException $ TestException 0)
            pure eventReference
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        shouldSatisfyNamed
          m_ev
          "was finalized with a manual exception"
          (maybe False (\ev → ev.err == (Just . EqSomeException $ SomeException $ TestException 0)))

  describe "addEventField" $
    it "adds a field to the event" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let
            ?e11yBackend = be
          idx ← withEvent Test $ do
            addEventField TestField
            pure eventReference
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        shouldSatisfyNamed
          m_ev
          "was finalized and has a TestField field"
          (maybe False (\ev → ev.fields == DataEventTestSelectorTestFields [TestField]))
  describe "DataEventBackend" $ do
    it "captures the first event finalization" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let ?e11yBackend = be
          idx ← withEvent Test $ do
            finalizeEvent Nothing
            finalizeEvent $ Just (SomeException $ TestException 0)
            pure eventReference
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        shouldSatisfyNamed
          m_ev
          "was finalized without error"
          (maybe False (\ev → isNothing ev.err))

    it "emits incomplete events as Nothing" $
      let
        (parentIdx ∷ Int, m_ev) = runST $ do
          be ← newDataEventBackend
          let ?e11yBackend = be
          withEvent Test $ do
            let parentIdx' = eventReference
            withEvent SubTest $ do
              finalizeEvent Nothing
              evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
              pure (parentIdx', index evs eventReference)
       in
        case m_ev of
          Nothing → expectationFailure "child event not finalized"
          Just ev →
            shouldSatisfyNamed
              ev
              "parent is unknown"
              (\ev' → ev'.parent == Just (Left parentIdx))

  describe "withRelatedEvent" $
    it "sets specified causes" $
      let
        m_cause_effect = runST $ do
          be ← newDataEventBackend
          let ?e11yBackend = be
          (causeIdx, effectIdx) ← do
            withEvent Test $ do
              let causeIdx = eventReference
              withRelatedEvent SubTest Nothing [causeIdx] $ do
                pure (causeIdx, eventReference)
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure (index evs causeIdx, index evs effectIdx)
       in
        case m_cause_effect of
          (Just cause, Just effect) →
            shouldSatisfyNamed
              (cause, effect)
              "fst causes snd"
              (\(cause', effect') → effect'.causes == [Right cause'])
          (Nothing, _) → expectationFailure "cause not finalized"
          (_, Nothing) → expectationFailure "effect not finalized"

  describe "instantEvent" $
    it "emits an event" $
      let
        m_ev = runST $ do
          be ← newDataEventBackend
          let ?e11yBackend = be
          idx ← instantEvent Test [TestField]
          evs ← (fmap convDataEventTestSelector <$>) <$> getEvents be
          pure $ index evs idx
       in
        case m_ev of
          Nothing → expectationFailure "event not finalized"
          Just ev →
            shouldSatisfyNamed
              ev
              "is instant has a TestField field"
              (\ev' → ev'.instant && ev'.fields == DataEventTestSelectorTestFields [TestField])
