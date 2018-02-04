{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker where

import Control.Distributed.Process (ProcessId, Process)
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.RWS.Strict

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict


import Queue -- imported from the library Okasaki

import Messages
import Spec
import Types


newtype WorkerAction t m a = WorkerAction {
                                 runApp :: ReaderT WorkerConfig (WriterT [Messages] (StateT (WorkerState t m) IO)) a
                                 } deriving  (Functor,
                                              Applicative,
                                              Monad,
                                              MonadState (WorkerState t m),
                                              MonadWriter [Messages],
                                              MonadReader WorkerConfig)

initWorker :: WorkerState a b -> IO (InChan a, OutChan a)
initWorker (WorkerState _ l) = newChan l

submitWork :: a -> IO Bool
submitWork = undefined

taskSubmissionHandler :: Messages -> WorkerAction t m ()
taskSubmissionHandler (WorkerTask _ _ t ) = undefined-- do
  -- WorkerState q l  <- get
  -- put $ WorkerState (t >>| q) (l + 1)

reportState :: WorkerAction t m ()
reportState = do
  WorkerConfig m me _ <- ask
  WorkerState _ l    <- get
  if (l == 0)
    then tell [WorkerCapacity me m Empty 0]
    else tell [WorkerCapacity me m Processing l]

-- This function actually applies the function to the data
-- The function will be applied on REPA arrays
-- apply :: (Num a) => Task a b -> FilePath -> IO ()
-- apply (Map f) filepath = do
--   let func = deRefStaticPtr f
--   let k = func 10
--   return ()

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

