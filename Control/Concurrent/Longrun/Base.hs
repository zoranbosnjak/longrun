-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Base
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Base).
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@via.si>
--
-- This file is part of Longrun.
--
-- Longrun is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Longrun is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Longrun. If not, see <http://www.gnu.org/licenses/>.
--
-----------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Longrun.Base
    ( module Control.Concurrent.Longrun.Base
    , Priority(..)
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Set as Set
import Data.Time
import System.Log.Logger (Priority(..))
import qualified System.Log.Logger as Log

type ProcName = String

data ProcConfig = ProcConfig
    { procName      :: [ProcName]
    , procChilds    :: TVar (Set Child)
    }

data ProcState = ProcState [Child]

data Child = forall a . (Terminator a) => Child a

-- | An interface for items that can be terminated.
class Terminator a where
    getTid :: a -> ThreadId
    terminate :: a -> IO ()

instance Eq Child where
    Child a == Child b = getTid a == getTid b

instance Ord Child where
    compare (Child a) (Child b) = compare (getTid a) (getTid b)

instance Terminator Child where
    getTid (Child a) = getTid a
    terminate (Child a) = terminate a

type Process a = ReaderT ProcConfig IO a

traceIO :: String -> IO ()
traceIO _s = do
    -- to console
#ifdef traceConsole
    putStrLn _s
#endif
    return ()

trace :: String -> Process ()
trace _s = do
    -- to console
#ifdef traceConsole
    liftIO $ putStrLn _s
#endif
    -- to logging
#ifdef traceLog
    logM DEBUG _s
#endif
    return ()

-- | Forever implementation from Control.Monad in combination with transformers 
-- has some problem with memory leak, use this version instead
forever :: Monad m => m a -> m b
forever act = act >> forever act

-- | Delay for a number of seconds
threadDelaySec :: Double -> IO ()
threadDelaySec sec = threadDelay . round . (1000000*) $ sec

-- | Delay for a number of seconds (Process version)
sleep :: Double -> Process ()
sleep sec = do
    trace $ "sleep " ++ show sec ++ " seconds"
    liftIO $ threadDelaySec sec

-- Log message via logging module.
logM :: Priority -> String -> Process ()
logM prio s = do
    name <- asks procName
    now <- liftIO $ Data.Time.getCurrentTime
    liftIO $ do
        let timeFormatUTC = "%Y-%m-%dT%H:%M:%SZ"
            _timeFormatLocal = "%Y-%m-%dT%H:%M:%S"
            timeFormatUnixEpoch = "%s.%q"
        let nowUtc = formatTime defaultTimeLocale timeFormatUTC now
            nowSec = formatTime defaultTimeLocale timeFormatUnixEpoch now
            f [] = ""
            f x = foldr1 (\a b -> a++"."++b) (reverse x)
        Log.logM (f name) prio $ s ++ " @ " ++ nowUtc ++ " (" ++ nowSec ++ ")"

-- | Raise action to a new level of process name.
group :: ProcName -> Process a -> Process a
group name action = do
    trace $ "group: " ++ show name
    local f action 
    where
        f cfg = cfg {procName = (name:(procName cfg))}

getChilds :: Process [Child]
getChilds = do
    var <- asks procChilds
    childs <- liftIO $ atomically $ readTVar var
    trace $ "getChilds, number: " ++ show (length (Set.toList childs))
    return $ Set.toList childs

modifyChilds :: (Set Child -> Set Child) -> Process ()
modifyChilds f = do
    var <- asks procChilds
    liftIO $ atomically $ do
        childs <- readTVar var
        writeTVar var $ f childs

-- | Add child to process config.
addChild :: Child -> Process ()
addChild child = do
    trace "addChild"
    modifyChilds $ Set.insert child

-- | Remove child from process config.
removeChild :: Child -> Process ()
removeChild child = do
    trace "removeChild"
    modifyChilds $ Set.delete child

-- | Convert IO action to Process.
runIO :: IO a -> Process a
runIO = liftIO

-- | Terminate self.
die :: Process ()
die = do
    trace "die"
    liftIO $ (Control.Concurrent.myThreadId >>= Control.Concurrent.killThread)

-- | Run application.
runApp :: Process a -> IO a
runApp app = do
    cfg <- liftIO emptyConfig
    runProcess cfg app

-- | Run process in the IO monad
runProcess :: ProcConfig -> Process a -> IO a
runProcess cfg action = process `Control.Exception.finally` cleanup where
    process = runReaderT action cfg
    cleanup = do
        childs <- atomically $ readTVar (procChilds cfg)
        traceIO $ "cleanup, num. childs: " ++ show (Set.size childs)
        mapM_ terminate $ Set.toList childs

-- | Create empty configuration.
emptyConfig :: IO ProcConfig
emptyConfig = do
    var <- newTVarIO Set.empty
    return $ ProcConfig 
        { procName = []
        , procChilds = var
        }

-- | Aquire resources, run action, release resources (bracket wrapper).
bracket :: Process res -> (res -> Process b) -> (res -> Process c) -> Process c
bracket aquire release action = do
    cfg <- ask
    let run = runProcess cfg
        aquire' = run aquire
        release' = run . release
        action' = run . action
    runIO $ Control.Exception.bracket aquire' release' action'

-- | Run action, then cleanup (finally wrapper).
finally :: Process a -> Process b -> Process a
finally action cleanup = do
    cfg <- ask
    let run = runProcess cfg
        action' = run action
        cleanup' = run cleanup
    runIO $ Control.Exception.finally action' cleanup'

-- Empty operation.
nop :: Process ()
nop = return ()

-- Do nothing (forever).
rest :: Process ()
rest = do
    trace "rest"
    forever $ liftIO $ threadDelaySec 1

