-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Base
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Subprocess).
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

module Control.Concurrent.Longrun.Subprocess
( Subprocess(Subprocess)
, spawnTask
, waitCatch
, spawnProcess
, spawnProcess_
, stop
, stop_
) where

import Control.Concurrent
    (myThreadId, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (SomeException)
import Control.Concurrent (throwTo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Control.Concurrent.Async as Async

import Control.Concurrent.Longrun.Base

-- | A subprocess will automatically terminate when parent terminates.
newtype Subprocess a = Subprocess (Async.Async a)

instance IsChild (Subprocess a) where
    asChild (Subprocess a) = Child
        { childTid = Async.asyncThreadId a
        , terminate = Async.cancel a
        }

-- | Spawn a child process that eventually returns something.
-- Use waitCatch to get the returned value or error.
spawnTask :: Process a -> Process (Subprocess a)
spawnTask action = do
    pName <- asks procName
    cfg <- mkChildConfig pName
    lock <- liftIO $ newEmptyMVar
    a <- liftIO $ Async.async $ do
        -- Need to wait for parent to finish updating its state
        _ <- takeMVar lock
        runProcess cfg action
    addChild $ asChild (Subprocess a)
    liftIO $ putMVar lock ()
    return $ Subprocess a

-- | Wait for a process to teminate.
waitCatch :: Subprocess a -> Process (Either SomeException a)
waitCatch (Subprocess a) = do
    rv <- liftIO $ Async.waitCatch a
    removeChild $ asChild (Subprocess a)
    return rv

-- | Spawn a subprocess that shall not terminate by itself.
spawnProcess :: Process () -> Process (Subprocess ())
spawnProcess action = do
    name <- asks procName
    parent <- liftIO $ Control.Concurrent.myThreadId
    spawnTask $ do
        a <- spawnTask action
        _ <- waitCatch a
        liftIO $ throwTo parent (ProcessTerminated name)

-- | Spawn a subprocess, ignore return value.
spawnProcess_ :: Process () -> Process ()
spawnProcess_ action = spawnProcess action >> return ()

-- | Stop a subprocess, remove it from the list of children.
-- Return running status, just before call to stop.
stop :: Subprocess a -> Process Bool
stop (Subprocess a) = do
    removeChild $ asChild (Subprocess a)
    liftIO $ terminate $ asChild (Subprocess a)
    rv <- liftIO $ Async.waitCatch a
    case rv of
        Left _ -> return True
        Right _ -> return False

-- | Stop a subprocess, don't care about running state.
stop_ :: Subprocess a -> Process ()
stop_ p = stop p >> return ()
