-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Timer
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Timer).
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

module Control.Concurrent.Longrun.Timer
( Timer(..)
, newTimer
, stopTimer
, stopTimer_
, restartTimer
, restartTimer_
, expireTimer
, expireTimer_
) where

import Control.Concurrent.MVar
import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess

data Timer = Timer
    { tSema     :: MVar ()
    , tParent   :: ThreadId
    , tTimeout  :: Double
    , tAction   :: Process ()
    , tRunning  :: STM.TVar (Maybe (Subprocess ()))
    , tExpired  :: STM.TVar Bool
    }

-- | Create new timer.
newTimer :: Double -> Process () -> Process Timer
newTimer seconds action = do
    sema <- liftIO $ newMVar ()
    parent <- liftIO $ Control.Concurrent.myThreadId
    running <- liftIO $ STM.newTVarIO Nothing
    expired <- liftIO $ STM.newTVarIO False
    return $ Timer
        { tSema = sema
        , tParent = parent
        , tTimeout = seconds
        , tAction = action
        , tRunning = running
        , tExpired = expired
        }

_withSemaphore :: Timer -> Process a -> Process a
_withSemaphore t action = bracket takeSema releaseSema action' where
    takeSema = liftIO $ takeMVar (tSema t)
    releaseSema _ = liftIO $ putMVar (tSema t) ()
    action' _ = action

-- | Stop timer.
_stopTimer :: Timer -> Process Bool
_stopTimer t = do
    (running, expired) <- liftIO $ STM.atomically $ do
        running <- STM.readTVar $ tRunning t
        expired <- STM.readTVar $ tExpired t
        STM.writeTVar (tRunning t) Nothing
        STM.writeTVar (tExpired t) False
        return (running, expired)
    case running of
        Nothing -> return False
        Just a -> do
            stop_ a
            return (not expired)

stopTimer :: Timer -> Process Bool
stopTimer t = _withSemaphore t (_stopTimer t)

stopTimer_ :: Timer -> Process ()
stopTimer_ t = stopTimer t >> return ()

-- | (Re)start timer.
restartTimer :: Timer -> Process Bool
restartTimer t = _withSemaphore t $ do
    parent <- liftIO $ Control.Concurrent.myThreadId
    wasRunning <- _stopTimer t
    -- start delayed task
    d <- spawnTask $ do
        sleep $ tTimeout t
        liftIO $ STM.atomically $ STM.writeTVar (tExpired t) True
        mask_ $ tAction t `onFailureSignal` parent

    liftIO $ STM.atomically $ STM.writeTVar (tRunning t) $ Just d
    return wasRunning

restartTimer_ :: Timer -> Process ()
restartTimer_ t = restartTimer t >> return ()

-- | Expedite timer expire if running.
expireTimer :: Timer -> Process Bool
expireTimer t = _withSemaphore t $ do
    parent <- liftIO $ Control.Concurrent.myThreadId
    wasRunning <- _stopTimer t
    when wasRunning $ do
        _ <- spawnTask $ do
            mask_ $ tAction t `onFailureSignal` parent
        return ()
    return wasRunning

expireTimer_ :: Timer -> Process ()
expireTimer_ t = expireTimer t >> return ()

