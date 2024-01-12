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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Exception
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.Catch.Pure qualified as Catch
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.With
import Data.Either
import Data.GeneralAllocate
import Data.Kind
import Data.Sequence
import Observe.Event
import Observe.Event.Backend (EventBackend (..), LiftBackend (..))
import Observe.Event.Data
import Test.Syd

data TestSelector ∷ Type → Type where
  Test ∷ TestSelector TestField

data TestField = TestField deriving (Eq)

type instance SubSelector TestField = SubTestSelector

data SubTestSelector ∷ Type → Type where
  SubTest ∷ SubTestSelector SubTestField

data SubTestField = SubTestField deriving (Eq)

type instance SubSelector SubTestField = NoEventsSelector

instance Eq (DataEvent TestSelector) where
  de@(DataEvent (Leaf Test) _ fields1) == DataEvent (Leaf Test) e2 fields2 = show (de.err) == show e2 && fields1 == fields2
  DataEvent (Test :/ Leaf SubTest) e1 fields1 == DataEvent (Test :/ Leaf SubTest) e2 fields2 = show e1 == show e2 && fields1 == fields2
  _ == _ = False

newtype NewCatchT m a = NewCatchT {runNewCatchT ∷ CatchT m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, Catch.MonadThrow)

deriving newtype instance (MonadWith m) ⇒ MonadWith (NewCatchT m)

deriving via LiftBackend (DataEventBackend m selector) instance (PrimMonad m) ⇒ EventBackend (NewCatchT m) (DataEventBackend m selector)

data TestException = TestException deriving (Show)

instance Exception TestException

main ∷ IO ()
main = sydTest $ do
  describe "withEvent" $ do
    it "creates the event" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          pure ()
        elem (DataEvent (Leaf Test) Nothing empty) <$> getEvents be
    it "sets the backend to accept sub-selectors in the inner scope" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          withEvent SubTest $ do
            addEventField SubTestField
        elem (DataEvent (Test :/ Leaf SubTest) Nothing (singleton SubTestField)) <$> getEvents be
    it "records and propagates exceptions" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        e_res ← runCatchT . runNewCatchT $ withEvent Test $ do
          Catch.throwM TestException
        case e_res of
          Left e → case fromException e of
            Just TestException →
              elem (DataEvent (Leaf Test) (Just (SomeException TestException)) empty) <$> getEvents be
            Nothing → pure False
          _ → pure False
  describe "LiftBackend" $
    it "lifts the EventBackend instance through a transformer" $
      runST $ do
        be ← newDataEventBackend @_ @TestSelector
        let
          ?e11yBackend = be
        e_res ← runCatchT . runNewCatchT $ withEvent Test $ do
          addEventField TestField
          pure True
        pure $ fromRight False e_res
  describe "finalizeEvent" $
    it "finalizes the event" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          finalizeEvent $ Just (SomeException TestException)
        elem (DataEvent (Leaf Test) (Just (SomeException TestException)) empty) <$> getEvents be
  describe "addEventField" $
    it "adds a field to the event" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          addEventField TestField
        elem (DataEvent (Leaf Test) Nothing (singleton TestField)) <$> getEvents be
  describe "DataEventBackend" $
    it "captures the first event finalization" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          finalizeEvent Nothing
          finalizeEvent $ Just (SomeException TestException)
        notElem (DataEvent (Leaf Test) (Just (SomeException TestException)) empty) <$> getEvents be
