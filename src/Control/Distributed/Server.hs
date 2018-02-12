module Control.Distributed.Server where

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Distributed.Process

data ServerConfig = ServerConfig
    { myId :: ProcessId
    , peers :: [ProcessId]
    } deriving (Show)

initServer :: ServerConfig -> IO ()
initServer config = undefined
