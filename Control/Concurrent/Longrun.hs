-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling.
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

module Control.Concurrent.Longrun
    ( readQueueTimeout
    , module Control.Concurrent.Longrun.Base
    , module Control.Concurrent.Longrun.Queue
    , module Control.Concurrent.Longrun.Subprocess
    , module Control.Concurrent.Longrun.Timer
    , module Control.Concurrent.Longrun.Variable
    ) where

import Control.Concurrent.Async (async, cancel)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess
import Control.Concurrent.Longrun.Variable
import Control.Concurrent.Longrun.Queue
import Control.Concurrent.Longrun.Timer


-- | Return (Just msg) or Nothing on timeout.
readQueueTimeout :: (ReadableQueue q a) => q a -> Double -> Process (Maybe a)
readQueueTimeout q timeout = do
    ready <- liftIO $ do
        expired <- STM.newTVarIO False
        task <- async $ do
            threadDelaySec timeout
            STM.atomically $ STM.writeTVar expired True
        ready <- STM.atomically $ do
            val <- tryPeekQueue q
            end <- STM.readTVar expired
            case (end, val) of
                (True, _) -> return False
                (False, Just _) -> return True
                _ -> STM.retry
        cancel task
        return ready
    case ready of
        False -> return Nothing
        True -> Just <$> readQueue q
