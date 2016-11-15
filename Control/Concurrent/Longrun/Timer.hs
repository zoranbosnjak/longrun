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

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Control.Concurrent.Longrun.Timer where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess

data Timer = 
    Timer !ProcName !Double !(Process ()) !(MVar (Maybe (Subprocess ())))

-- | Create new timer.
newTimer :: ProcName -> Double -> Process () -> Process Timer
newTimer name seconds action = group name $ do
    trace $ "newTimer, seconds: " ++ show seconds
    var <- runIO $ newMVar Nothing
    return $ Timer name seconds action var

-- | (Re)start timer.
restartTimer :: Timer -> Process Bool
restartTimer (Timer name seconds action var) = group name $ do
    parent <- liftIO $ Control.Concurrent.myThreadId

    -- take MVar, stop timer
    running <- do
        ma <- runIO $ takeMVar var
        case ma of
            Nothing -> return False
            Just a -> do
                stop a
                return True

    -- start delayed task, put MVar
    d <- spawnTask "delayed" $ ungroup $ do
        sleep seconds
        _ <- runIO $ takeMVar var
        trace "timer expired"
        a <- spawnTask "action" $ ungroup $ action
        rv <- waitCatch a
        runIO $ putMVar var Nothing
        case rv of
            Left e -> liftIO $ Control.Concurrent.throwTo parent e
            Right _ -> return ()
    runIO $ putMVar var $ Just d

    trace $ "restartTimer, was running: " ++ show running
    return running

-- | Stop timer.
stopTimer :: Timer -> Process Bool
stopTimer (Timer name _seconds _action var) = group name $ do
    running <- do
        ma <- runIO $ takeMVar var
        case ma of
            Nothing -> return False
            Just a -> do
                stop a
                return True
    runIO $ putMVar var Nothing
    trace $ "stopTimer, was running: " ++ show running
    return running

-- | Expedite timer expire if running.
expireTimer :: Timer -> Process Bool
expireTimer t@(Timer name _seconds action _var) = group name $ do
    running <- ungroup $ stopTimer t
    when running action
    trace $ "expireTimer, was running: " ++ show running
    return running

