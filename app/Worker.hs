{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker where

import Control.Distributed.Process (ProcessId, Process)
import Control.Distributed.Process.Serializable
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.RWS.Strict

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Queue
-- Limiting the kind of tasks possible right now but will extend this in future to include all kinds of Haskell tasks
-- TODO: Look at LINQ operators for this function
-- a is the serialized function to be applied here Eg: (+ 1)
data Task a = Map a
            | Filter a
            | Reduce a
            | GroupBy a
            deriving (Show, Generic, Typeable)

data QueueState = Empty
                | Processing
                deriving (Show, Generic, Typeable)

-- TODO: This should be moved to a separate file Messages.hs
-- a is the serialized function to be applied
data Messages a = WorkerCapacity { senderOf    :: ProcessId
                                 , recipientOf :: ProcessId
                                 , msg         :: QueueState
                                 , workLoad    :: Int
                                 }
                -- the worker pushes this every n secs to the scheduler.
                | WorkerTask { senderOf    :: ProcessId
                             , recipientOf :: ProcessId
                             , work        :: Task a}
                -- the master sends this to the worker every time a worker returns that it is empty
                deriving (Show, Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worker to peers

data WorkerConfig = WorkerConfig { master      :: ProcessId
                                 , myId        :: ProcessId
                                 , peers       :: [ProcessId] -- this will be useful for work stealing later
                                 }

data WorkerState a = WorkerState { taskQueue   :: Queue (Task a)
                                 , queueLength :: Int
                                 }



newtype WorkerAction m a = WorkerAction {runAction :: RWS WorkerConfig [Messages m] (WorkerState m) a
                                        } deriving (Functor,
                                                    Applicative,
                                                    Monad,
                                                    MonadState (WorkerState m),
                                                    MonadWriter [Messages m],
                                                    MonadReader WorkerConfig)


initWorker :: WorkerState a -> IO (InChan a, OutChan a)
initWorker (WorkerState _ l) = newChan l

submitWork :: a -> IO Bool
submitWork = undefined

taskSubmissionHandler :: Messages m -> WorkerAction m ()
taskSubmissionHandler (WorkerTask _ _ t ) = do
  WorkerState q l  <- get
  put $ WorkerState (t >>| q) (l + 1)

reportState :: WorkerAction m ()
reportState = do
  WorkerConfig m me _ <- ask
  WorkerState _ l    <- get
  if (l == 0)
    then tell [WorkerCapacity me m Empty 0]
    else tell [WorkerCapacity me m Processing l]

-- This function actually applies the function to the data
foo :: Task a -> FilePath -> IO ()
foo = undefined

test :: IO ()
test = do
  (inC, outC) <- newChan 5
  writeChan inC 6
  writeChan inC 7
  writeChan inC 8
  x <- readChan outC
  print x
  n <- estimatedLength inC
  print n
  return ()

