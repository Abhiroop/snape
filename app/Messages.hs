{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Control.Distributed.Process (ProcessId)

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import GHC.StaticPtr

import Types

-- This module defines all the message types possible for the master as well as the worker as well as any other classification of the node defined in the future.

data Messages = WorkerCapacity { senderOf    :: ProcessId
                               , recipientOf :: ProcessId
                               , msg         :: WorkerQueueState
                               , workLoad    :: Int
                               }
                -- the worker pushes this every n secs to the scheduler.
                | WorkerTask { senderOf    :: ProcessId
                             , recipientOf :: ProcessId
                             , work        :: StaticKey}
                -- the master sends this to the worker every time a worker returns that it is empty
                deriving (Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worker to peers

instance Binary Messages
