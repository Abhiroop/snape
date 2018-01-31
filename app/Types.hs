{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Distributed.Process (ProcessId)

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)


import Queue

import Spec

-- Worker's types

data WorkerQueueState = Empty
                      | Processing
                      deriving (Show, Generic, Typeable)

instance Binary WorkerQueueState

data WorkerConfig = WorkerConfig { master      :: ProcessId
                                 , myId        :: ProcessId
                                 , peers       :: [ProcessId] -- this will be useful for work stealing later
                                 }

data WorkerState a b = WorkerState { taskQueue   :: Queue (Task a b)
                                 , queueLength :: Int
                                 }
