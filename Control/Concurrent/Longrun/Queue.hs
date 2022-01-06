-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Queue
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Queue).
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Concurrent.Longrun.Queue
( ReadEnd, WriteEnd
, newQueue, newUnboundedQueue, newBoundedQueue, newBoundedQueue1
, IsQueue(..)
, readQueueBlocking
, writeQueueBlocking
) where

import Numeric.Natural
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.Longrun.Base

data Queue a
    = QueueUnbounded (STM.TQueue a)
    | QueueBounded (STM.TBQueue a)

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

-- | Create new queue, return both ends of the queue.
newQueue :: Maybe Natural -> Process (WriteEnd a, ReadEnd a)
newQueue mBound = do
    q <- case mBound of
        Nothing -> QueueUnbounded <$> (liftIO $ STM.newTQueueIO)
        Just bound -> QueueBounded <$> (liftIO $ STM.newTBQueueIO bound)
    return (WriteEnd q, ReadEnd q)

newUnboundedQueue :: Process (WriteEnd a, ReadEnd a)
newUnboundedQueue = newQueue Nothing

newBoundedQueue :: Natural -> Process (WriteEnd a, ReadEnd a)
newBoundedQueue bound = newQueue (Just bound)

newBoundedQueue1 :: Process (WriteEnd a, ReadEnd a)
newBoundedQueue1 = newBoundedQueue 1

class IsQueue q a where
    -- | Get STM.T*Queue out of the structure
    qQueue :: q a -> Either (STM.TQueue a) (STM.TBQueue a)

instance IsQueue Queue a where
    qQueue (QueueUnbounded q) = Left q
    qQueue (QueueBounded q) = Right q

instance IsQueue ReadEnd a where
    qQueue (ReadEnd (QueueUnbounded q)) = Left q
    qQueue (ReadEnd (QueueBounded q)) = Right q

instance IsQueue WriteEnd a where
    qQueue (WriteEnd (QueueUnbounded q)) = Left q
    qQueue (WriteEnd (QueueBounded q)) = Right q

-- | Read from queue (blocking).
readQueueBlocking :: ReadEnd a -> Process a
readQueueBlocking = liftIO . STM.atomically . stmRead . qQueue
  where
    stmRead (Left stmQ) = STM.readTQueue stmQ
    stmRead (Right stmQ) = STM.readTBQueue stmQ

-- | Write to queue (blocking for bounded queue).
writeQueueBlocking :: WriteEnd a -> a -> Process ()
writeQueueBlocking q val = liftIO $ STM.atomically $ stmWrite (qQueue q) val
  where
    stmWrite (Left stmQ) = STM.writeTQueue stmQ
    stmWrite (Right stmQ) = STM.writeTBQueue stmQ

