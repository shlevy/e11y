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
import Observe.Event
import Observe.Event.Backend (EventBackend (..), LiftBackend (..))
import Observe.Event.Data
import Test.Syd

data TestSelector ∷ Type → Type where
  Test ∷ TestSelector TestField

data TestField

type instance SubSelector TestField = SubTestSelector

data SubTestSelector ∷ Type → Type where
  SubTest ∷ SubTestSelector SubTestField

data SubTestField

type instance SubSelector SubTestField = NoEventsSelector

instance Eq (DataEvent TestSelector) where
  de@(DataEvent (Leaf Test) _) == DataEvent (Leaf Test) e2 = show (err de) == show e2
  DataEvent (Test :/ Leaf SubTest) e1 == DataEvent (Test :/ Leaf SubTest) e2 = show e1 == show e2
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
        elem (DataEvent (Leaf Test) Nothing) <$> getEvents be
    it "sets the backend to accept sub-selectors in the inner scope" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          withEvent SubTest $ do
            pure ()
        elem (DataEvent (Test :/ Leaf SubTest) Nothing) <$> getEvents be
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
              elem (DataEvent (Leaf Test) (Just (SomeException TestException))) <$> getEvents be
            Nothing → pure False
          _ → pure False
  describe "LiftBackend" $
    it "lifts the EventBackend instance through a transformer" $
      runST $ do
        be ← newDataEventBackend @_ @TestSelector
        let
          ?e11yBackend = be
        e_res ← runCatchT . runNewCatchT $ withEvent Test $ do
          pure True
        pure $ fromRight False e_res
  describe "earlyFinalize" $
    it "finalizes the event" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          earlyFinalize $ Just (SomeException TestException)
        elem (DataEvent (Leaf Test) (Just (SomeException TestException))) <$> getEvents be
  describe "DataEventBackend" $
    it "captures the first event finalization" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          earlyFinalize Nothing
          earlyFinalize $ Just (SomeException TestException)
        notElem (DataEvent (Leaf Test) (Just (SomeException TestException))) <$> getEvents be
