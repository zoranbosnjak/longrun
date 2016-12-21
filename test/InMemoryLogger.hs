module InMemoryLogger
( InMemoryLogger
, inMemoryLogger
, evacuateLogger
) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_)
import System.Log.Handler
import System.Log (Priority (DEBUG), LogRecord)
import System.Log.Formatter (LogFormatter, nullFormatter)

data InMemoryLogger = InMemoryLogger
    { _priority :: Priority
    , _formatter :: LogFormatter InMemoryLogger
    , _entries :: MVar [LogRecord]
    }

instance LogHandler InMemoryLogger where
    getLevel logger = _priority logger

    setLevel logger p = logger { _priority = p }

    setFormatter logger f = logger { _formatter = f }

    emit logger record _ = modifyMVar_ (_entries logger) $ \entries ->
      return $ record : entries

    close _ = return ()

inMemoryLogger :: IO InMemoryLogger
inMemoryLogger = do
    entries <- newMVar []
    return  $ InMemoryLogger
        { _priority = DEBUG
        , _formatter = nullFormatter
        , _entries = entries
        }


evacuateLogger :: InMemoryLogger -> IO [LogRecord]
evacuateLogger logger = modifyMVar (_entries logger) $ \entries ->
  return ([], reverse entries)

