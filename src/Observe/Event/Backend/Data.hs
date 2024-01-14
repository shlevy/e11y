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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Description : Consuming events as data
Copyright   : Copyright 2024 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module provides the t'DataEventBackend' 'EventBackend' for consuming events
by representing them as ordinary Haskell data.
-}
module Observe.Event.Backend.Data
  ( newDataEventBackend
  , getEvents
  , DataEvent (..)
  , Selectors (..)
  , DataEventBackend
  )
where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Functor.Parametric
import Data.Primitive.MutVar
import Data.Sequence as Seq
import Observe.Event.Backend

{- | An 'EventBackend' for consuming events by representing them as
ordinary Haskell data.

Create a new one with 'newDataEventBackend'. Get the event data with 'getEvents'.
-}
newtype DataEventBackend m selector = DataEventBackend (MutVar (PrimState m) (Seq (MutVar (PrimState m) (Maybe (PendingDataEvent selector)))))

-- | Allocate a new t'DataEventBackend'
newDataEventBackend ∷ ∀ m selector. (PrimMonad m, ParametricFunctor m) ⇒ m (DataEventBackend m selector)
newDataEventBackend = coerce $ newMutVar @m (empty @(MutVar (PrimState m) (Maybe (PendingDataEvent selector))))

{- | Read the events that have been emitted using the t'DataEventBackend'

A 'Nothing' indicates an event that hasn't been 'finalize'd.
-}
getEvents
  ∷ (PrimMonad m)
  ⇒ DataEventBackend m selector
  → m (Seq (Maybe (DataEvent selector)))
getEvents eb = do
  pendingEvVars ← readMutVar $ coerce eb
  pendingEvs ← traverse readMutVar pendingEvVars
  let
    res = unPend <$> pendingEvs
    find n = case index res n of
      Just ev → Right ev
      Nothing → Left n
    unPend (Just ev@(PendingDataEvent _ selectors _ _ _ fields _)) =
      Just $
        DataEvent
          { idx = ev.reference
          , parent = find <$> ev.parent
          , causes = find <$> ev.causes
          , selectors
          , err = ev.err
          , fields
          , instant = ev.instant
          }
    unPend Nothing = Nothing
  pure res

-- | A representation of an event.
data DataEvent selector = ∀ f.
  DataEvent
  { idx ∷ !Int
  -- ^ This event's index in the sequence of event creations.
  , selectors ∷ !(Selectors selector f)
  -- ^ The [selector](Observe-Event.html#g:selectorAndField) used to initialize the event
  , parent ∷ !(Maybe (Either Int (DataEvent selector)))
  -- ^ The [parent](Observe-Event.html#g:relationships) of this t'DataEvent', if any.
  --
  -- @Left n@ means the parent was the n'th event but wasn't 'finalize'd.
  , causes ∷ ![Either Int (DataEvent selector)]
  -- ^ The [causes](Observe-Event.html#g:relationships) of this t'DataEvent'.
  --
  -- @Left n@ means the cause was the n'th event but wasn't 'finalize'd.
  , err ∷ !(Maybe SomeException)
  -- ^ The error which ended the event, if any
  , fields ∷ !(Seq f)
  -- ^ The fields which were added to the event
  , instant ∷ !Bool
  -- ^ Whether the event was emitted instantly.
  }

-- | An in-progress representation of an event.
data PendingDataEvent selector = ∀ f.
  PendingDataEvent
  { reference ∷ !Int
  , selectors ∷ !(Selectors selector f)
  , parent ∷ !(Maybe Int)
  , causes ∷ ![Int]
  , err ∷ !(Maybe SomeException)
  , fields ∷ !(Seq f)
  , instant ∷ !Bool
  }

-- | The 'Event' associated with t'DataEventBackend'.
data DataEventBackendEvent m selector f = DataEventBackendEvent
  { reference ∷ !Int
  , cell ∷ !(MutVar (PrimState m) (Maybe (PendingDataEvent selector)))
  , params ∷ !(EventParams selector f Int)
  , fields ∷ !(MutVar (PrimState m) (Seq f))
  }

-- | Consume events by representing them as ordinary Haskell data.
instance Event (DataEventBackendEvent m selector) where
  type EventReference (DataEventBackendEvent m selector) = Int
  reference ev = ev.reference

-- | Consume events by representing them as ordinary Haskell data.
instance (PrimMonad m) ⇒ EventIn m (DataEventBackendEvent m selector) where
  finalize ev err = do
    fields ← readMutVar ev.fields
    let
      modify ∷ Maybe (PendingDataEvent selector) → (Maybe (PendingDataEvent selector), ())
      modify Nothing =
        ( Just $
            PendingDataEvent
              { reference = ev.reference
              , selectors = ev.params.selectors
              , parent = ev.params.parent
              , causes = ev.params.causes
              , err
              , fields
              , instant = False
              }
        , ()
        )
      modify n = (n, ())
    atomicModifyMutVar' ev.cell modify
  addField ev f =
    atomicModifyMutVar' ev.fields $ \fs → (fs |> f, ())

-- | Consume events by representing them as ordinary Haskell data.
instance EventBackend (DataEventBackend m selector) where
  type BackendEvent (DataEventBackend m selector) = DataEventBackendEvent m selector
  type RootSelector (DataEventBackend m selector) = selector

newCell ∷ (PrimMonad m) ⇒ DataEventBackend m selector → m (MutVar (PrimState m) (Maybe (PendingDataEvent selector)), Int)
newCell eb = do
  cell ← newMutVar Nothing
  ref ← atomicModifyMutVar' (coerce eb) (\evs → (evs |> cell, Seq.length evs))
  pure (cell, ref)

-- | Consume events by representing them as ordinary Haskell data.
instance (PrimMonad m) ⇒ EventBackendIn m (DataEventBackend m selector) where
  newEvent eb params = do
    (cell, ref) ← newCell eb
    fields ← newMutVar $ Seq.fromList params.initialFields
    pure DataEventBackendEvent{fields, reference = ref, params, cell}
  newInstantEvent eb params = do
    (cell, ref) ← newCell eb
    writeMutVar cell . Just $
      PendingDataEvent
        { reference = ref
        , selectors = params.selectors
        , parent = params.parent
        , causes = params.causes
        , err = Nothing
        , fields = Seq.fromList params.initialFields
        , instant = True
        }
    pure ref

deriving via
  LiftBackendEvent (DataEventBackend m selector)
  instance
    (EventBackendIn m' (DataEventBackend m selector), MonadTrans t, ParametricFunctor (t m'))
    ⇒ EventIn (t m') (DataEventBackendEvent m selector)

deriving via
  LiftBackend (DataEventBackend m selector)
  instance
    (EventBackendIn m' (DataEventBackend m selector), MonadTrans t, ParametricFunctor (t m'), ParametricFunctor m')
    ⇒ EventBackendIn
        (t m')
        (DataEventBackend m selector)
