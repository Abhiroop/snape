{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Distributed.Worker where

import Control.Distributed.Process (ProcessId, Process, send, match, receiveWait)
import Control.Concurrent.Chan.Unagi.Bounded

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import GHC.StaticPtr
import System.IO.Unsafe

import Control.Distributed.Messages
import Control.Distributed.Types
import Data.Spec

newtype WorkerAction t m a = WorkerAction {
                                 runApp :: ReaderT WorkerConfig (WriterT [Messages] (StateT (WorkerState t m) IO)) a
                                 } deriving  (Functor,
                                              Applicative,
                                              Monad,
                                              MonadIO,
                                              MonadState (WorkerState t m),
                                              MonadWriter [Messages],
                                              MonadReader WorkerConfig)

runWorker :: WorkerConfig -> WorkerState a b -> WorkerAction a b c -> IO ([Messages],WorkerState a b)
runWorker config state m = runStateT (execWriterT $ runReaderT (runApp m) config) state

-- TODO: Spawn a new process which would call `reportState`
runServer :: WorkerConfig -> WorkerState a b -> Process ()
runServer wc ws = do
  let run handler msg = liftIO $ runWorker wc ws (handler msg)
  (messages,state') <- receiveWait [
    match $ run taskSubmissionHandler ,
    match $ run statusReportHandler
                                   ]
  mapM_ (\msg -> send (recipientOf msg) msg) messages
  runServer wc state'

initWorker :: Int -> WorkerState a b -> IO (InChan a, OutChan a)
initWorker l (WorkerState _) = newChan l

taskSubmissionHandler :: Messages -> WorkerAction t m ()
taskSubmissionHandler (WorkerTask _ _ t ) = do
  WorkerState q <- get
  task <- liftIO $ unsafeLookupStaticPtr t
  case task of
    Just t  -> liftIO $ writeToQ (deRefStaticPtr t) q
    Nothing -> return ()

statusReportHandler :: Messages -> WorkerAction t m ()
statusReportHandler (StatusReport sender _)= do
  WorkerConfig master me _ <- ask
  WorkerState q       <- get
  (inC,_) <- liftIO $ q
  l <- liftIO $ estimatedLength inC
  case sender == master of
    False -> return ()
    True -> if (l == 0)
            then tell [WorkerCapacity me sender Empty 0]
            else tell [WorkerCapacity me sender Processing l]
