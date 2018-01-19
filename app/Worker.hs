{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker where

import Control.Distributed.Process (ProcessId)
import Control.Distributed.Process.Serializable
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.RWS.Strict

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Queue
-- Limiting the kind of tasks possible right now but will extend this in future to include all kinds of Haskell tasks
-- TODO: Look at LINQ operators for this function
data Task = Map
          | Filter
          | Reduce
          | GroupBy
          deriving (Show, Generic, Typeable)

data QueueState = Empty
                | Processing
                deriving (Show, Generic, Typeable)

-- TODO: This should be moved to a separate file Messages.hs
data Messages = WorkerCapacity { senderOf    :: ProcessId
                               , recipientOf :: ProcessId
                               , msg         :: QueueState
                               , workLoad    :: Int
                               }
                | WorkerTask { senderOf    :: ProcessId
                             , recipientOf :: ProcessId
                             , work        :: Task}
                deriving (Show, Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worket to peers

data WorkerConfig = WorkerConfig { master      :: ProcessId
                                 , myId        :: ProcessId
                                 , peers       :: [ProcessId] -- this will be useful for work stealing later
                                 }

data WorkerState = WorkerState { taskQueue   :: Queue Task
                               , queueLength :: Int
                               }

newtype WorkerAction a = WorkerAction {runAction :: RWS WorkerConfig [Messages] WorkerState a
                                      } deriving (Functor,
                                                  Applicative,
                                                  Monad,
                                                  MonadState WorkerState,
                                                  MonadWriter [Messages],
                                                  MonadReader WorkerConfig)

initWorker :: WorkerState -> IO (InChan a, OutChan a)
initWorker (WorkerState _ l) = newChan l

submitWork :: a -> IO Bool
submitWork = undefined

taskSubmissionHandler :: Messages -> WorkerAction ()
taskSubmissionHandler (WorkerTask _ _ t ) = do
  WorkerState q l  <- get
  put $ WorkerState (t >>| q) (l + 1)

reportState :: WorkerAction ()
reportState = do
  WorkerConfig m me _ <- ask
  WorkerState _ l    <- get
  if (l == 0)
    then tell [WorkerCapacity me m Empty 0]
    else tell [WorkerCapacity me m Processing l]


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

