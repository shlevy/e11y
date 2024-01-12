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
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.With
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
  DataEvent (Leaf Test) == DataEvent (Leaf Test) = True
  DataEvent (Test :/ Leaf SubTest) == DataEvent (Test :/ Leaf SubTest) = True
  _ == _ = False

newtype NewIdentityT f a = NewIdentityT {runNewIdentityT ∷ f a}
  deriving newtype (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

deriving newtype instance (MonadWith f) ⇒ MonadWith (NewIdentityT f)

deriving via LiftBackend (DataEventBackend m selector) instance (PrimMonad m) ⇒ EventBackend (NewIdentityT m) (DataEventBackend m selector)

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
        elem (DataEvent (Leaf Test)) <$> getEvents be
    it "sets the backend to accept sub-selectors in the inner scope" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        withEvent Test $ do
          withEvent SubTest $ do
            pure ()
        elem (DataEvent (Test :/ Leaf SubTest)) <$> getEvents be
  describe "LiftBackend" $
    it "lifts the EventBackend instance through a transformer" $
      runST $ do
        be ← newDataEventBackend @_ @TestSelector
        let
          ?e11yBackend = be
        runNewIdentityT $ withEvent Test $ do
          pure True
