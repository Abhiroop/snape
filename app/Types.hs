{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Distributed.Process (ProcessId)
import Control.Concurrent.Chan.Unagi.Bounded
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

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

data WorkerState a b = WorkerState { taskQueue   :: IO (InChan (Task a b), OutChan (Task a b))
                                   , queueLength :: Int
                                   }

writeToQ :: Task a b -> IO (InChan (Task a b), OutChan (Task a b)) -> IO ()
writeToQ t q = do
  (inC,_) <- q
  writeChan inC t
