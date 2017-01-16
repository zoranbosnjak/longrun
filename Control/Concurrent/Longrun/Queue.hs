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
, readEnd
, readQueue
, tryPeekQueue
, writeEnd
, writeQueue
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.Longrun.Base

data Queue a = Queue
    { qName     :: ProcName
    , qRead     :: STM a
    , qWrite    :: a -> STM ()
    , qTryPeek  :: STM (Maybe a)
    }

newtype ReadEnd a = ReadEnd (Queue a)
newtype WriteEnd a = WriteEnd (Queue a)

readEnd :: Queue a -> ReadEnd a
readEnd = ReadEnd

writeEnd :: Queue a -> WriteEnd a
writeEnd = WriteEnd

queueName :: Queue a -> ProcName
queueName = qName

-- | Create new queue.
newQueue :: Maybe Int -> ProcName -> Process (Queue a)
newQueue mBound name = group name $ case mBound of
    Nothing -> newUnboundedQueue name
    Just bound -> newBoundedQueue name bound

newUnboundedQueue :: ProcName -> Process (Queue a)
newUnboundedQueue name = do
    trace $ "newQueue (unbounded)"
    q <- liftIO $ STM.newTQueueIO
    return $ Queue name
                   (STM.readTQueue q)
                   (STM.writeTQueue q)
                   (STM.tryPeekTQueue q)

newBoundedQueue :: ProcName -> Int -> Process (Queue a)
newBoundedQueue name bound = do
    trace $ "newQueue (bounded " ++ show bound ++ ")"
    q <- liftIO $ STM.newTBQueueIO bound
    return $ Queue name
                   (STM.readTBQueue q)
                   (STM.writeTBQueue q)
                   (STM.tryPeekTBQueue q)

-- | Create one element bounded queue.
newQueue1 :: ProcName -> Process (Queue a)
newQueue1 = newQueue (Just 1)

class ReadableQueue q a where
    readQueue :: q a -> Process a
    tryPeekQueue :: q a -> STM (Maybe a)

instance (Show a) => ReadableQueue Queue a where
    readQueue q = group (qName q) $ do
        val <- liftIO $ STM.atomically $ qRead q
        trace $ "readQueue, value: " ++ show val
        return val
    tryPeekQueue = qTryPeek

instance (Show a) => ReadableQueue ReadEnd a where
    readQueue (ReadEnd q) = readQueue q
    tryPeekQueue (ReadEnd q) = tryPeekQueue q


class WriteableQueue q a where
    writeQueue :: q a -> a -> Process ()

instance (Show a, NFData a) => WriteableQueue Queue a where
    writeQueue q val = group (qName q) $ do
        val' <- force val
        liftIO $ STM.atomically $ (qWrite q) val'
        trace $ "writeQueue, value: " ++ show val'

instance (Show a, NFData a) => WriteableQueue WriteEnd a where
    writeQueue (WriteEnd q) val = writeQueue q val
