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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.Catch.Pure qualified as Catch
import Control.Monad.ST
import Control.Monad.StrictIdentity
import Control.Monad.Trans.Class
import Control.Monad.With
import Data.Coerce
import Data.Either
import Data.Foldable
import Data.Functor.Identity
import Data.GeneralAllocate
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Sequence
import Observe.Event
import Observe.Event.Backend.Data
import Test.Syd

deriving via WithNoContinuation StrictIdentity instance MonadWith StrictIdentity

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

data ParentSelector field where
  ParentA ∷ ParentSelector AField
  ParentB ∷ ParentSelector BField

newtype AField = AField Int deriving (Eq)

type instance SubSelector AField = NoEventsSelector

data BField = BYes | BNo deriving (Eq)

type instance SubSelector BField = ChildSelector

data ChildSelector field where
  Child ∷ ChildSelector CField

data CField = CField deriving (Eq)

type instance SubSelector CField = NoEventsSelector

data SomeSelectors = ∀ field. (Eq field) ⇒ SomeSelectors (Selectors ParentSelector field)

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

  describe "the MonadTrans EventBackendIn instance" $
    it "lifts the EventBackend instance through a transformer" $
      runST $ do
        be ← newDataEventBackend @_ @TestSelector
        let
          ?e11yBackend = be
        e_res ← runCatchT $ withEvent Test $ do
          addEventField TestField
          subRef ← instantEvent SubTest []
          evs ← (fmap convDataEventTestSelector <$>) <$> lift (getEvents be)
          pure $ case index evs subRef of
            Nothing → False
            Just ev → ev.fields == DataEventTestSelectorSubTestFields []

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
        -- The parentIdx type annotation is needed because its
        -- type inside the block is EventReference (DataEventBackend (ST s) TestSelector.
        -- s cannot escape from runST, and even though that type is always Int regardless
        -- of s GHC doesn't know that.

        -- The m_ev type annotation is needed because of https://gitlab.haskell.org/ghc/ghc/-/issues/24333
        (parentIdx ∷ Int, m_ev ∷ Maybe DataEventTestSelector) = runST $ do
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

  describe "the no-op EventBackend instance for Proxy" $
    it "does nothing successfully" $
      runStrictIdentity $ do
        let ?e11yBackend = Proxy @TestSelector
        () ← withEvent Test $ do
          addEventField TestField
          let !_ = eventReference
          finalizeEvent Nothing
          instantEvent SubTest [SubTestField]
        pure True

  describe "the backend-combining EventBackend instance for (,)" $
    it "sequentially calls both backends" $
      let
        (evs1, evs2, idx1s ∷ (Int, Int), idx2s ∷ (Int, Int)) = runST $ do
          be1 ← newDataEventBackend
          be2 ← newDataEventBackend
          let ?e11yBackend = (be1, be2)
          (idx1, idx2) ← withEvent Test $ do
            addEventField TestField
            let idx1 = eventReference
            idx2 ← instantEvent SubTest [SubTestField]
            finalizeEvent Nothing
            pure (idx1, idx2)
          (,,idx1,idx2)
            <$> ((fmap convDataEventTestSelector <$>) <$> getEvents be1)
            <*> ((fmap convDataEventTestSelector <$>) <$> getEvents be2)
        (idx11, idx12) = idx1s
        (idx21, idx22) = idx2s
        expectedEv1 =
          DataEventTestSelector
            { idx = 0
            , selector = DataEventTestSelectorTest
            , parent = Nothing
            , causes = []
            , err = Nothing
            , fields = DataEventTestSelectorTestFields [TestField]
            , instant = False
            }
        expectedEv2 =
          DataEventTestSelector
            { idx = 1
            , selector = DataEventTestSelectorTestSubTest
            , parent = Just (Right expectedEv1)
            , causes = []
            , err = Nothing
            , fields = DataEventTestSelectorSubTestFields [SubTestField]
            , instant = True
            }
       in
        do
          shouldBe idx11 idx12
          shouldBe idx21 idx22
          evs ←
            if evs1 == evs2
              then pure evs1
              else expectationFailure "event lists don't match"
          shouldBe evs $ fromList [Just expectedEv1, Just expectedEv2]
  describe "selectorRendering" $
    it "matches a manual definition" $
      let
        possibleSelectors =
          [ SomeSelectors (Leaf ParentA)
          , SomeSelectors (Leaf ParentB)
          , SomeSelectors (ParentB :/ Leaf Child)
          ]
        manual ∷ Selectors ParentSelector field → field
        manual (Leaf ParentA) = AField 0
        manual (ParentA :/ Leaf impossible) = case impossible of {}
        manual (ParentA :/ impossible :/ _) = case impossible of {}
        manual (Leaf ParentB) = BYes
        manual (ParentB :/ Leaf Child) = CField
        manual (ParentB :/ Child :/ Leaf impossible) = case impossible of {}
        manual (ParentB :/ Child :/ impossible :/ _) = case impossible of {}

        usingSelectorRendering ∷ Selectors ParentSelector field → field
        usingSelectorRendering =
          runIdentity
            . selectorRendering
              ( \case
                  ParentA → noSubEventsSelectorRendering (Identity $ AField 0)
                  ParentB →
                    SelectorRendering
                      { renderTopSelector = Identity BYes
                      , renderSubSelector = renderChildSelector
                      }
              )
        renderChildSelector ∷ Selectors ChildSelector field → Identity field
        renderChildSelector = selectorRendering $ \Child →
          noSubEventsSelectorRendering $ Identity CField

        predicate ∷ SomeSelectors → Bool
        predicate (SomeSelectors sel) = manual sel == usingSelectorRendering sel
       in
        all predicate possibleSelectors
