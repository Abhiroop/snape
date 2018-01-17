module Worker where

import Control.Concurrent.Chan.Unagi.Bounded

type QueueLength = Int

initWorker :: QueueLength -> IO ()
initWorker n = undefined
