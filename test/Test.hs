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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.ST
import Control.Monad.With
import Data.Kind
import Observe.Event
import Observe.Event.Data
import Test.Syd

data TestSelector ∷ Type → Type where
  Test ∷ TestSelector ()

instance Eq (DataEvent TestSelector) where
  DataEvent Test == DataEvent Test = True

instrumentedTest ∷ (HasEvents m h TestSelector, MonadWith m) ⇒ m ()
instrumentedTest = withEvent Test $ do
  pure ()

main ∷ IO ()
main = sydTest $ do
  describe "withEvent" $
    it "creates the event" $
      runST $ do
        be ← newDataEventBackend
        let
          ?e11yBackend = be
        instrumentedTest
        elem (DataEvent Test) <$> getEvents be
