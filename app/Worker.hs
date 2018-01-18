{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker where

import Control.Distributed.Process (ProcessId)
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.RWS.Strict

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data QueueState = Empty | Processing
  deriving (Show, Generic, Typeable)

data MessageTypes = MyCapacity { senderOf    :: ProcessId
                               , recipientOf :: ProcessId
                               , msg         :: QueueState
                               , workLoad    :: Int
                               } deriving (Show, Generic, Typeable)
-- add more messages in futures
-- 1. work stealing from worket to peers

data WorkerConfig = WorkerConfig { master      :: ProcessId
                                 , myId        :: ProcessId
                                 , peers       :: [ProcessId] -- this will be useful for work stealing later
                                 }

data WorkerState = forall a . WorkerState { taskQueue   :: IO (InChan a, OutChan a)
                                          , queueLength :: Int
                                          }

newtype WorkerAction a = WorkerAction {runAction :: RWS WorkerConfig [MessageTypes] WorkerState a
                                      } deriving (Functor,
                                                  Applicative,
                                                  Monad,
                                                  MonadState WorkerState,
                                                  MonadWriter [MessageTypes],
                                                  MonadReader WorkerConfig)

initWorker :: WorkerState -> IO (InChan a, OutChan a)
initWorker (WorkerState _ l) = newChan l

submitWork :: a -> IO Bool
submitWork = undefined

reportState :: WorkerAction ()
reportState = do
  WorkerConfig m me _ <- ask
  WorkerState _ l    <- get
  if (l == 0)
    then tell [MyCapacity me m Empty 0]
    else tell [MyCapacity me m Processing l]


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

