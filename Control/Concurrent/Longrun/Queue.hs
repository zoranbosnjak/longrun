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

module Control.Concurrent.Longrun.Queue where

import Control.DeepSeq (NFData)
import Control.Concurrent.STM
import Control.Monad.Trans.Reader

import Control.Concurrent.Longrun.Base

data Queue a
    = QUnbounded ProcNames (TQueue a)
    | QBounded ProcNames (TBQueue a)

data ReadEnd a = ReadEnd (Queue a)
data WriteEnd a = WriteEnd (Queue a)

-- | Create new queue.
newQueue :: Maybe Int -> ProcName -> Process (Queue a)
newQueue mBound name = group name $ do
    qName <- asks procName
    case mBound of
        Nothing -> do
            trace $ "newQueue (unbounded) " ++ show qName
            q <- runIO $ newTQueueIO
            return $ QUnbounded qName q
        Just bound -> do
            trace $ "newQueue (bounded: " ++ show bound ++ ") " ++ show qName
            q <- runIO $ newTBQueueIO bound
            return $ QBounded qName q

-- | Create one element bounded queue.
newQueue1 :: ProcName -> Process (Queue a)
newQueue1 = newQueue (Just 1)

-- | Read data from the queue.
readQueue :: (Show a) => ReadEnd a -> Process a
readQueue (ReadEnd q) = do
    (name,val) <- runIO $ atomically $ case q of
        QUnbounded name q' -> readTQueue q' >>= \val -> return (name, val)
        QBounded name q' -> readTBQueue q' >>= \val -> return (name, val)
    trace $ "readQueue " ++ show name ++ ", value: " ++ show val
    return val

-- | Read data from the queue (operate on Queue instead of ReadEnd).
readQueue' :: (Show a) => Queue a -> Process a
readQueue' = readQueue . ReadEnd

-- | Write data to the queue.
writeQueue :: (Show a, NFData a) => WriteEnd a -> a -> Process ()
writeQueue (WriteEnd q) val = do
    val' <- force val
    name <- runIO $ atomically $ case q of
        QUnbounded name q' -> writeTQueue q' val' >> return name
        QBounded name q' -> writeTBQueue q' val' >> return name
    trace $ "writeQueue " ++ show name ++ ", value: " ++ show val'

-- | Write data to the queue (operate on Queue instead of WriteEnd)
writeQueue' :: (Show a, NFData a) => Queue a -> a -> Process ()
writeQueue' = writeQueue . WriteEnd

