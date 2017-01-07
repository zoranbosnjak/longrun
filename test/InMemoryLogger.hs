module InMemoryLogger
( InMemoryLogger
, inMemoryLogger
, evacuateLogger
, logInMemory
) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_)
import System.Log (Priority, LogRecord)

newtype InMemoryLogger = InMemoryLogger
    { _entries :: MVar [LogRecord]
    }

inMemoryLogger :: IO InMemoryLogger
inMemoryLogger = do
    entries <- newMVar []
    return $ InMemoryLogger entries

evacuateLogger :: InMemoryLogger -> IO [LogRecord]
evacuateLogger logger = modifyMVar (_entries logger) $ \entries ->
  return ([], reverse entries)

logInMemory :: InMemoryLogger -> String -> Priority -> String -> IO ()
logInMemory logger _ priority msg = modifyMVar_ (_entries logger) $ \entries ->
  return $ (priority, msg) : entries
