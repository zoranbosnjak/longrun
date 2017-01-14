-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Variable
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Variable).
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

module Control.Concurrent.Longrun.Variable
( GetEnd
, SetEnd
, Var(Var)
, getEnd
, getVar
, modifyVar
, newVar
, newVarBind
, onChangeVar
, setEnd
, setVar
, varName
) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base
import Control.Concurrent.Longrun.Subprocess (spawnProcess)

data Var a = Var ProcName (STM.TVar a)

varName :: Var a -> ProcName
varName (Var name _) = name

newtype GetEnd a = GetEnd (Var a)
newtype SetEnd a = SetEnd (Var a)

getEnd :: Var a -> GetEnd a
getEnd = GetEnd

setEnd :: Var a -> SetEnd a
setEnd = SetEnd

-- | Create new variable.
newVar :: (Show a) => ProcName -> a -> Process (Var a)
newVar name val = group name $ do
    trace $ "newVar, initial: " ++ show val
    var <- liftIO $ STM.newTVarIO val
    return $ Var name var

-- | Bind existing TVar to a variable
newVarBind :: (Show a) => ProcName -> (STM.TVar a) -> Process (Var a)
newVarBind name var = group name $ do
    val <- liftIO $ STM.atomically $ STM.readTVar var
    trace $ "newVarBind, initial: " ++ show val
    return $ Var name var

class GettableVar v a where
    getVar :: v a -> Process a

instance (Show a) => GettableVar Var a where
    getVar (Var name var) = group name $ do
        val <- liftIO $ STM.atomically $ STM.readTVar var
        trace $ "getVar, value: " ++ show val
        return val

instance (Show a) => GettableVar GetEnd a where
    getVar (GetEnd v) = getVar v


class SettableVar v a where
    setVar :: v a -> a -> Process ()

instance (Show a, NFData a) => SettableVar Var a where
    setVar (Var name var) val = group name $ do
        val' <- force val
        trace $ "setVar, value: " ++ show val'
        liftIO $ STM.atomically $ STM.writeTVar var val'

instance (Show a, NFData a) => SettableVar SetEnd a where
    setVar (SetEnd v) = setVar v


-- | Modify variable content.
modifyVar :: (Show a, NFData a) => Var a -> (a -> a) -> Process (a,a)
modifyVar (Var name var) f = group name $ do
    (oldValue, newValue) <- liftIO $ STM.atomically $ do
        a <- STM.readTVar var
        STM.modifyTVar var f
        b <- STM.readTVar var
        return (a,b)
    newValue' <- force newValue
    trace $ "modifyVar: " ++ show oldValue ++ " -> " ++ show newValue'
    return (oldValue, newValue')


-- | Run single action on each variable change.
onChangeVar :: (Eq b) =>
    String -> b -> GetEnd a -> (a->b) -> (b -> b -> Process ()) -> Process Child
onChangeVar procname initial (GetEnd (Var varname var)) f act = group procname $
    do
        trace $ "onChangeVar " ++ show varname

        -- We want to block indefinitely if variable is never changed
        -- need another dummy thread to reference 'var',
        -- just to prevent deadlock detection.
        -- TODO: replace this ugly solution to keep var reference alive
        _ <- spawnProcess "dummy" nop $ forever $ do
            sleep $ case (var==var) of
                True -> 1
                False -> 2
        p <- ungroup $ spawnProcess procname nop $ loop initial
        return $ asChild p
      where
        loop x = do
            y <- liftIO $ STM.atomically $ do
                y <- STM.readTVar var >>= return . f
                case y == x of
                    True -> STM.retry
                    False -> return y
            trace $ "variable " ++ show varname ++ " changed, triggering action"
            act x y
            loop y
