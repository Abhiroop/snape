{-# LANGUAGE DeriveGeneric #-}
module Control.Distributed.Messages where

import Control.Distributed.Process (ProcessId)

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import GHC.StaticPtr

import Control.Distributed.Types

-- This module defines all the message types possible for the master as well as the worker as well as any other classification of the node defined in the future.

data Messages = WorkerCapacity { senderOf    :: ProcessId
                               , recipientOf :: ProcessId
                               , msg         :: WorkerQueueState
                               , workLoad    :: Int
                               }

                | WorkerTask { senderOf    :: ProcessId
                             , recipientOf :: ProcessId
                             , work        :: StaticKey}

                | StatusReport { senderOf    :: ProcessId
                               , recipientOf :: ProcessId}
                deriving (Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worker to peers

instance Binary Messages
