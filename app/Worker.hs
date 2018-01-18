module Worker where

import Control.Concurrent.Chan.Unagi.Bounded

type QueueLength = Int

data WorkerState a = WorkerState { taskQueue :: (InChan a, OutChan a)
                                 }

initWorker :: QueueLength -> IO (InChan a, OutChan a)
initWorker = newChan

submitWork :: a -> IO Bool
submitWork = undefined

test :: IO ()
test = do
  (inC, outC) <- newChan 5 -- :: IO (InChan a, OutChan a)
  writeChan inC 6
  writeChan inC 7
  writeChan inC 8
  x <- readChan outC
  print x
  n <- estimatedLength inC
  print n
  return ()

