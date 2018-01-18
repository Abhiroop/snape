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

type QueueLength = Int

data QueueState = Empty | Processing
  deriving (Show, Generic, Typeable)

data Message = Message { senderOf    :: ProcessId
                       , recipientOf :: ProcessId
                       , msg         :: QueueState
                       } deriving (Show, Generic, Typeable)

data WorkerConfig = WorkerConfig { master      :: ProcessId
                                 , myId        :: ProcessId
                                 , peers       :: [ProcessId] -- this will be useful for work stealing later
                                 , queueLength :: Integer
                                 }

data WorkerState = forall a . WorkerState { taskQueue :: IO (InChan a, OutChan a)
                                          }

newtype WorkerAction a = WorkerAction {runAction :: RWS WorkerConfig [Message] WorkerState a
                                      } deriving (Functor,
                                                  Applicative,
                                                  Monad,
                                                  MonadState WorkerState,
                                                  MonadWriter [Message],
                                                  MonadReader WorkerConfig)

initWorker :: QueueLength -> IO (InChan a, OutChan a)
initWorker = newChan

submitWork :: a -> IO Bool
submitWork = undefined

--askForWork :: 
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

