{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Worker where

import Control.Distributed.Process (ProcessId, Process)
import Control.Concurrent.Chan.Unagi.Bounded

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import GHC.StaticPtr

import Messages
import Spec
import Types


newtype WorkerAction t m a = WorkerAction {
                                 runApp :: ReaderT WorkerConfig (WriterT [Messages] (StateT (WorkerState t m) IO)) a
                                 } deriving  (Functor,
                                              Applicative,
                                              Monad,
                                              MonadIO,
                                              MonadState (WorkerState t m),
                                              MonadWriter [Messages],
                                              MonadReader WorkerConfig)

initWorker :: Int -> WorkerState a b -> IO (InChan a, OutChan a)
initWorker l (WorkerState _) = newChan l

taskSubmissionHandler :: Messages -> WorkerAction t m ()
taskSubmissionHandler (WorkerTask _ _ t ) = do
  WorkerState q <- get
  task <- liftIO $ unsafeLookupStaticPtr t
  case task of
    Just t -> liftIO $ writeToQ (deRefStaticPtr t) q
    Nothing -> put $ WorkerState q

reportState :: WorkerAction t m ()
reportState = do
  WorkerConfig m me _ <- ask
  WorkerState q       <- get
  (inC,_) <- liftIO $ q
  l <- liftIO $ estimatedLength inC
  if (l == 0)
    then tell [WorkerCapacity me m Empty 0]
    else tell [WorkerCapacity me m Processing l]
