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
, ReadableQueue
, WriteEnd
, WriteableQueue
, newQueue
, newQueue1
, queueName
, queueQueue
, readEnd
, readQueue
, readQueueTimeout
, writeEnd
, writeQueue
, writeQueueTimeout
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.Longrun.Base

import Control.Concurrent.Async (async, cancel, pollSTM)

data Queue a = Queue
    { qName     :: ProcName
    , qQueue    :: Either (STM.TQueue a) (STM.TBQueue a)
    }

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

readEnd :: Queue a -> ReadEnd a
readEnd = ReadEnd

writeEnd :: Queue a -> WriteEnd a
writeEnd = WriteEnd

queueName :: Queue a -> ProcName
queueName = qName

queueQueue :: Queue a -> Either (STM.TQueue a) (STM.TBQueue a)
queueQueue = qQueue

qRead :: Queue a -> STM a
qRead (Queue _ (Left q)) = STM.readTQueue q
qRead (Queue _ (Right q)) = STM.readTBQueue q

qTryRead :: Queue a -> STM (Maybe a)
qTryRead (Queue _ (Left q)) = STM.tryReadTQueue q
qTryRead (Queue _ (Right q)) = STM.tryReadTBQueue q

qWrite :: Queue a -> a -> STM ()
qWrite (Queue _ (Left q)) val = STM.writeTQueue q val
qWrite (Queue _ (Right q)) val = STM.writeTBQueue q val

qIsFull :: Queue a -> STM Bool
qIsFull (Queue _ (Left _)) = return False
qIsFull (Queue _ (Right q)) = STM.isFullTBQueue q

-- | Create new queue.
newQueue :: Maybe Int -> ProcName -> Process (Queue a)
newQueue mBound name = group name $ case mBound of
    Nothing -> newUnboundedQueue name
    Just bound -> newBoundedQueue name bound

newUnboundedQueue :: ProcName -> Process (Queue a)
newUnboundedQueue name = do
    trace $ "newQueue (unbounded)"
    q <- liftIO $ STM.newTQueueIO
    return $ Queue name (Left q)

newBoundedQueue :: ProcName -> Int -> Process (Queue a)
newBoundedQueue name bound = do
    trace $ "newQueue (bounded " ++ show bound ++ ")"
    q <- liftIO $ STM.newTBQueueIO bound
    return $ Queue name (Right q)

-- | Create one element bounded queue.
newQueue1 :: ProcName -> Process (Queue a)
newQueue1 = newQueue (Just 1)

class ReadableQueue q a where
    readQueue :: q a -> Process a
    readQueueTimeout :: Double -> q a -> Process (Maybe a)

instance (Show a) => ReadableQueue Queue a where
    readQueue q = group (qName q) $ do
        val <- liftIO $ STM.atomically $ qRead q
        trace $ "readQueue, value: " ++ show val
        return val
    readQueueTimeout timeout q = group (qName q) $ do
        a <- liftIO $ async $ threadDelaySec timeout
        val <- liftIO $ STM.atomically $ do
            to <- pollSTM a
            mVal <- qTryRead q
            case mVal of
                Just val -> return $ Just val
                Nothing -> case to of
                    Nothing -> STM.retry
                    Just _ -> return Nothing
        liftIO $ cancel a
        trace $ "readQueueTimeout, value: " ++ show val
        return val

instance (Show a) => ReadableQueue ReadEnd a where
    readQueue (ReadEnd q) = readQueue q
    readQueueTimeout timeout (ReadEnd q) = readQueueTimeout timeout q


class WriteableQueue q a where
    writeQueue :: q a -> a -> Process ()
    writeQueueTimeout :: Double -> q a -> a -> Process Bool

instance (Show a, NFData a) => WriteableQueue Queue a where
    writeQueue q val = group (qName q) $ do
        val' <- force val
        liftIO $ STM.atomically $ (qWrite q) val'
        trace $ "writeQueue, value: " ++ show val'
    writeQueueTimeout timeout q val = group (qName q) $ do
        val' <- force val
        a <- liftIO $ async $ threadDelaySec timeout
        rv <- liftIO $ STM.atomically $ do
            to <- pollSTM a
            isFull <- qIsFull q
            case isFull of
                False -> do
                    qWrite q $ val'
                    return True
                True -> case to of
                    Nothing -> STM.retry
                    Just _ -> return False
        trace $
            "writeQueueTimeout, value: " ++ show val' ++ ", done: " ++ show rv
        return rv

instance (Show a, NFData a) => WriteableQueue WriteEnd a where
    writeQueue (WriteEnd q) val = writeQueue q val
    writeQueueTimeout timeout (WriteEnd q) val = writeQueueTimeout timeout q val

