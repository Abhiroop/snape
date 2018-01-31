{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Control.Distributed.Process (ProcessId)

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Types
import Spec

-- This module defines all the message types possible for the master as well as the worker as well as any other classification of the node defined in the future.

-- a -> b is the serialized function to be applied
data Messages a b = WorkerCapacity { senderOf    :: ProcessId
                                   , recipientOf :: ProcessId
                                   , msg         :: WorkerQueueState
                                   , workLoad    :: Int
                                   }
                -- the worker pushes this every n secs to the scheduler.
                | WorkerTask { senderOf    :: ProcessId
                             , recipientOf :: ProcessId
                             , work        :: Task a b}
                -- the master sends this to the worker every time a worker returns that it is empty
                deriving (Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worker to peers

-- TODO: Need to serialize the message before sending 
--instance (Binary a, Binary b) => Binary (Messages a b)
