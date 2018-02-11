module Control.Distributed.Server where

import Control.Distributed.Process
import Control.Concurrent.Chan.Unagi.Bounded

data ServerConfig = ServerConfig { myId  :: ProcessId
                                 , peers :: [ProcessId]
                                 } deriving (Show)

initServer :: ServerConfig -> IO ()
initServer config = undefined
