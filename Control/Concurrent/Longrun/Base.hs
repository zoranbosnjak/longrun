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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Concurrent.Longrun.Base
( AppConfig (AppConfig)
, Child (..)
, Control.Concurrent.Longrun.Base.bracket
, Control.Concurrent.Longrun.Base.finally
, Control.Concurrent.Longrun.Base.mask_
, Control.Concurrent.Longrun.Base.try
, IsChild(..)
, Priority(..)
, ProcConfig (ProcConfig)
, ProcName, ProcNames
, ProcError(..)
, Process
, addChild
, die
, forever
, getChildren
, group
, logM
, mkChildConfig
, nop
, procChildren
, procName
, removeChild
, rest
, runApp
, runAppWithConfig
, runProcess
, sleep
, threadDelaySec
, ungroup
) where

import Control.Concurrent (ThreadId, threadDelay, myThreadId, killThread)
import Control.Concurrent.STM
    (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Exception
    (Exception, bracket, finally, mask_, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Typeable (Typeable)
import Data.Set as Set
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Log.Logger
    (Priority(DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY))
import qualified System.Log.Logger as Log

data ProcError
    = ProcessTerminated ProcNames
    deriving (Eq, Show, Typeable)

instance Exception ProcError

type ProcName = String
type ProcNames = [ProcName]

type Logger = String -> Priority -> String -> IO ()

data ProcConfig = ProcConfig
    { procName      :: ProcNames
    , procChildren  :: TVar (Set Child)
    , procLogger    :: Logger
    }

data Child = Child
    { childTid :: ThreadId
    , terminate :: IO ()
    }

instance Eq Child where
    Child {childTid=t1} == Child {childTid=t2} = t1 == t2

instance Ord Child where
    Child {childTid=t1} `compare` Child {childTid=t2} = t1 `compare` t2

class IsChild a where
    asChild :: a -> Child

newtype Process a = Process (ReaderT ProcConfig IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProcConfig)

newtype AppConfig = AppConfig Logger

-- | Forever implementation from Control.Monad in combination with transformers
-- has some problem with memory leak, use this version instead.
-- Make sure to keep type signature "forever :: Process () -> Process ()".
-- For example, if the type signature is generalized to
-- "forever :: Monad m => m a -> m b",as suggested by ght,
-- the function still leaks memory.
forever :: Process () -> Process ()
forever act = act >> forever act

-- | Delay for a number of seconds
threadDelaySec :: Double -> IO ()
threadDelaySec sec = threadDelay $ round $ 1000000 * sec

-- | Delay for a number of seconds (Process version)
sleep :: Double -> Process ()
sleep sec = liftIO $ threadDelaySec sec

-- Log message via logging module.
logM :: Priority -> String -> Process ()
logM prio s = do
    name <- asks procName
    logger <- asks procLogger
    now <- liftIO $ Data.Time.getCurrentTime
    liftIO $ do
        let timeFormatUTC = "%Y-%m-%dT%H:%M:%SZ"
            _timeFormatLocal = "%Y-%m-%dT%H:%M:%S"
            timeFormatUnixEpoch = "%s.%q"
        let nowUtc = formatTime defaultTimeLocale timeFormatUTC now
            nowSec = formatTime defaultTimeLocale timeFormatUnixEpoch now
            f [] = ""
            f x = foldr1 (\a b -> a++"."++b) (reverse x)

        logger (f name) prio $! s ++ " @ " ++ nowUtc ++ " (" ++ nowSec ++ ")"

-- | Raise action to a new level of process name.
group :: ProcName -> Process a -> Process a
group name action = local f action where
    f cfg = cfg {procName = (name:(procName cfg))}

-- | Remove one level of process name.
ungroup :: Process a -> Process a
ungroup action = local f action where
   f cfg = cfg {procName = tail (procName cfg)}

-- | Get process children.
getChildren :: Process [Child]
getChildren = do
    var <- asks procChildren
    children <- liftIO $ atomically $ readTVar var
    return $ Set.toList children

_modifyChildren :: (Set Child -> Set Child) -> Process ()
_modifyChildren f = do
    var <- asks procChildren
    liftIO $ atomically $ modifyTVar' var f

-- | Add child to process config.
addChild :: Child -> Process ()
addChild child = _modifyChildren $ Set.insert child

-- | Remove child from process config.
removeChild :: Child -> Process ()
removeChild child = _modifyChildren $ Set.delete child

-- | Terminate self.
die :: Process ()
die = liftIO $ (Control.Concurrent.myThreadId >>= Control.Concurrent.killThread)

-- | Run application.
runApp :: Process a -> IO a
runApp = runAppWithConfig $ AppConfig Log.logM

-- | Run application.
runAppWithConfig :: AppConfig -> Process a -> IO a
runAppWithConfig (AppConfig logger) app = do
    cfg <- mkBaseConfig [] logger
    runProcess cfg app

-- | Run process in the IO monad.
runProcess :: ProcConfig -> Process a -> IO a
runProcess cfg (Process action) =
    process `Control.Exception.finally` cleanup
  where
    process = runReaderT action cfg
    cleanup = do
        children <- atomically $ readTVar (procChildren cfg)
        mapM_ terminate $ Set.toList children

-- | Create an empty configuration.
mkBaseConfig :: ProcNames -> Logger -> IO ProcConfig
mkBaseConfig names logger = do
    var <- newTVarIO Set.empty
    return $ ProcConfig
        { procName = names
        , procChildren = var
        , procLogger = logger
        }

-- | Create an empty configuration inheriting the logger.
mkChildConfig :: ProcNames -> Process ProcConfig
mkChildConfig names = do
    parentLogger <- asks procLogger
    liftIO $ mkBaseConfig names parentLogger

-- | Aquire resources, run action, release resources (bracket wrapper).
bracket :: Process res -> (res -> Process b) -> (res -> Process c) -> Process c
bracket aquire release action = do
    cfg <- do
        name <- asks procName
        mkChildConfig name
    let run = runProcess cfg
        aquire' = run aquire
        release' = run . release
        action' = run . action
    liftIO $ Control.Exception.bracket aquire' release' action'

-- | Run action, then cleanup (finally wrapper).
finally :: Process a -> Process b -> Process a
finally action cleanup = do
    cfg <- do
        name <- asks procName
        mkChildConfig name
    let run = runProcess cfg
        action' = run action
        cleanup' = run cleanup
    liftIO $ Control.Exception.finally action' cleanup'

-- | Try to run an action (try wrapper).
try :: Exception e => Process a -> Process (Either e a)
try action = do
    cfg <- do
        name <- asks procName
        mkChildConfig name
    liftIO $ Control.Exception.try (runProcess cfg action)

-- Empty operation.
nop :: Process ()
nop = return ()

-- Do nothing (forever).
rest :: Process ()
rest = forever $ liftIO $ threadDelaySec 1

-- | Protect process from being terminated while it's running.
mask_ :: Process a -> Process a
mask_ proc = do
    cfg <- do
        name <- asks procName
        mkChildConfig name
    liftIO $ Control.Exception.mask_ $ runProcess cfg proc

