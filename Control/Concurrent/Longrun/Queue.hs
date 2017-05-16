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
( Queue
, ReadEnd
, WriteEnd
, readEnd
, writeEnd
, newQueue
, newQueue1
, IsQueue(..)
, ReadableQueue(..)
, WriteableQueue(..)
) where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (async, cancel, pollSTM)

import Control.Concurrent.Longrun.Base

data Queue a = Queue (STM.TBQueue a)

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

readEnd :: Queue a -> ReadEnd a
readEnd = ReadEnd

writeEnd :: Queue a -> WriteEnd a
writeEnd = WriteEnd

-- | Create new queue.
newQueue :: Int -> Process (Queue a)
newQueue bound = Queue <$> (liftIO $ STM.newTBQueueIO bound)

newQueue1 :: Process (Queue a)
newQueue1 = newQueue 1

class IsQueue q a where
    qQueue :: q a -> STM.TBQueue a

instance IsQueue Queue a where
    qQueue (Queue q) = q

instance IsQueue ReadEnd a where
    qQueue (ReadEnd (Queue q)) = q

instance IsQueue WriteEnd a where
    qQueue (WriteEnd (Queue q)) = q

class ReadableQueue q a where
    readQueueBlocking :: q a -> Process a
    readQueueTimeout :: Double -> q a -> Process (Maybe a)

instance ReadableQueue Queue a where
    readQueueBlocking (Queue q) = liftIO $ STM.atomically $ STM.readTBQueue q
    readQueueTimeout timeout (Queue q) = do
        a <- liftIO $ async $ threadDelaySec timeout
        val <- liftIO $ STM.atomically $ do
            to <- pollSTM a
            mVal <- STM.tryReadTBQueue q
            case mVal of
                Just val -> return $ Just val
                Nothing -> case to of
                    Nothing -> STM.retry
                    Just _ -> return Nothing
        liftIO $ cancel a
        return val

instance ReadableQueue ReadEnd a where
    readQueueBlocking (ReadEnd q) = readQueueBlocking q
    readQueueTimeout timeout (ReadEnd q) = readQueueTimeout timeout q

class WriteableQueue q a where
    writeQueueBlocking :: q a -> a -> Process ()
    writeQueueTimeout :: Double -> q a -> a -> Process Bool

instance WriteableQueue Queue a where
    writeQueueBlocking (Queue q) val = liftIO $ STM.atomically $
        STM.writeTBQueue q val
    writeQueueTimeout timeout (Queue q) val = do
        a <- liftIO $ async $ threadDelaySec timeout
        rv <- liftIO $ STM.atomically $ do
            to <- pollSTM a
            isFull <- STM.isFullTBQueue q
            case isFull of
                False -> do
                    STM.writeTBQueue q val
                    return True
                True -> case to of
                    Nothing -> STM.retry
                    Just _ -> return False
        return rv

instance WriteableQueue WriteEnd a where
    writeQueueBlocking (WriteEnd q) val = writeQueueBlocking q val
    writeQueueTimeout timeout (WriteEnd q) val = writeQueueTimeout timeout q val

