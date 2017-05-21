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
, newQueue, newQueue1
, IsQueue(..)
, readQueueBlocking, readQueueTimeout
, writeQueueBlocking, writeQueueTimeout
) where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (async, cancel, pollSTM)

import Control.Concurrent.Longrun.Base

data Queue a = Queue (STM.TBQueue a)

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

-- | Create new bounded queue, return both ends of the queue.
newQueue :: Int -> Process (WriteEnd a, ReadEnd a)
newQueue bound = do
    q <- Queue <$> (liftIO $ STM.newTBQueueIO bound)
    return (WriteEnd q, ReadEnd q)

newQueue1 :: Process (WriteEnd a, ReadEnd a)
newQueue1 = newQueue 1

class IsQueue q a where
    -- | Get STM.TBQueue out of the structure
    qQueue :: q a -> STM.TBQueue a

instance IsQueue Queue a where
    qQueue (Queue q) = q

instance IsQueue ReadEnd a where
    qQueue (ReadEnd (Queue q)) = q

instance IsQueue WriteEnd a where
    qQueue (WriteEnd (Queue q)) = q

-- | Read from queue (blocking).
readQueueBlocking :: ReadEnd a -> Process a
readQueueBlocking = liftIO . STM.atomically . STM.readTBQueue . qQueue

-- | Read from queue (with timeout).
readQueueTimeout :: Double -> ReadEnd a -> Process (Maybe a)
readQueueTimeout timeout q = do
    a <- liftIO $ async $ threadDelaySec timeout
    val <- liftIO $ STM.atomically $ do
        to <- pollSTM a
        mVal <- STM.tryReadTBQueue $ qQueue q
        case mVal of
            Just val -> return $ Just val
            Nothing -> case to of
                Nothing -> STM.retry
                Just _ -> return Nothing
    liftIO $ cancel a
    return val

-- | Write to queue (blocking).
writeQueueBlocking :: WriteEnd a -> a -> Process ()
writeQueueBlocking q val =
    liftIO $ STM.atomically $ STM.writeTBQueue (qQueue q) val

-- | Write to queue (with timeout), return write success.
writeQueueTimeout :: Double -> WriteEnd a -> a -> Process Bool
writeQueueTimeout timeout q val = do
    a <- liftIO $ async $ threadDelaySec timeout
    rv <- liftIO $ STM.atomically $ do
        expired <- pollSTM a
        isFull <- STM.isFullTBQueue (qQueue q)
        case isFull of
            False -> do
                STM.writeTBQueue (qQueue q) val
                return True
            True -> case expired of
                Nothing -> STM.retry
                Just _ -> return False
    return rv

