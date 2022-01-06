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
, Logger
, noLogger
, printLogger
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
, onFailureSignal
) where

import Control.Concurrent
    (ThreadId, threadDelay, myThreadId, killThread, throwTo)
import Control.Concurrent.STM
    (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Exception
    (Exception, bracket, finally, mask_, try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Data.Typeable (Typeable)
import Data.Set as Set
import System.Log.Logger (Priority(..))

data ProcError
    = ProcessTerminated ProcNames
    deriving (Eq, Show, Typeable)

instance Exception ProcError

type ProcName = String
type ProcNames = [ProcName]

type Logger = ProcNames -> Priority -> String -> IO ()

-- | Drop log messages.
noLogger :: Logger
noLogger _app _prio _msg = return ()

-- | Print log messages.
printLogger :: Logger
printLogger app prio msg = print (app, prio, msg)

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

newtype Process a = Process { unProcess :: ReaderT ProcConfig IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader ProcConfig)

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

-- Log message (call external logger).
logM :: Priority -> String -> Process ()
logM prio s = do
    names <- asks procName
    logger <- asks procLogger
    liftIO $ logger names prio s

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
runApp :: Logger -> Process a -> IO a
runApp logger = runAppWithConfig $ AppConfig logger

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
bracket acquire release action = Process (ReaderT run) where
    run config = Control.Exception.bracket
                    (runReaderT (unProcess acquire) config)
                    (\res -> runReaderT (unProcess $ release res) config)
                    (\res -> runReaderT (unProcess $ action res) config)



-- | Run action, then cleanup (finally wrapper).
finally :: Process a -> Process b -> Process a
finally action cleanup = Process (ReaderT run) where
    action' = runReaderT $ unProcess action
    cleanup' = runReaderT $ unProcess cleanup
    run config = action' config `Control.Exception.finally` cleanup' config

-- | Try to run an action (try wrapper).
try :: Exception e => Process a -> Process (Either e a)
try (Process action) = Process (ReaderT run) where
    run config = Control.Exception.try $ runReaderT action config

-- Empty operation.
nop :: Process ()
nop = return ()

-- Do nothing (forever).
rest :: Process ()
rest = forever $ liftIO $ threadDelaySec 1

-- | Protect process from being terminated while it's running.
mask_ :: Process a -> Process a
mask_ proc = Process $ ReaderT run where
    run config = Control.Exception.mask_ $ runReaderT (unProcess proc) config

-- | Report failure (if any) to the process
onFailureSignal :: Process () -> ThreadId -> Process ()
onFailureSignal action proc = do
    rv <- Control.Concurrent.Longrun.Base.try action
    case rv of
        Left e -> liftIO $ throwTo proc (e::SomeException)
        Right _ -> return ()

