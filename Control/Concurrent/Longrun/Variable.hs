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

module Control.Concurrent.Longrun.Variable where

import Control.DeepSeq (NFData)
import Control.Concurrent.STM
import Control.Monad.Trans.Reader

import Control.Concurrent.Longrun.Base

data Var a = Var ProcNames (TVar a)
data GetEnd a = GetEnd (Var a)
data SetEnd a = SetEnd (Var a)

-- | Create new variable.
newVar :: (Show a) => String -> a -> Process (Var a)
newVar name val = group name $ do
    varName <- asks procName
    trace $ "newVar " ++ show varName ++ ", initial: " ++ show val
    var <- runIO $ newTVarIO val
    return $ Var varName var

-- | Bind existing TVar to a variable
newVarBind :: (Show a) => String -> (TVar a) -> Process (Var a)
newVarBind name var = group name $ do
    varName <- asks procName
    val <- runIO $ atomically $ readTVar var
    trace $ "newVarBind " ++ show varName ++ ", initial: " ++ show val
    return $ Var varName var

-- | Get variable content.
getVar :: (Show a) => GetEnd a -> Process a
getVar (GetEnd (Var name var)) = do
    val <- runIO $ atomically $ readTVar var
    trace $ "getVar " ++ show name ++ ", value: " ++ show val
    return val

-- | Get variable content (operate on Var instead on GetEnd)
getVar' :: (Show a) => Var a -> Process a
getVar' = getVar . GetEnd

-- | Set variable content (evaluated).
setVar :: (Show a, NFData a) => SetEnd a -> a -> Process ()
setVar (SetEnd (Var name var)) val = do
    val' <- force val
    trace $ "setVar " ++ show name ++ ", value: " ++ show val'
    runIO $ atomically $ writeTVar var val'
 
-- | Set variable content (operate on Var instead of SetEnd)
setVar' :: (Show a, NFData a) => Var a -> a -> Process ()
setVar' = setVar . SetEnd

-- | Modify variable content.
modifyVar :: (Show a, NFData a) => Var a -> (a -> a) -> Process (a,a)
modifyVar (Var name var) f = do
    (oldValue, newValue) <- runIO $ atomically $ do
        a <- readTVar var
        modifyTVar var f
        b <- readTVar var
        return (a,b)
    newValue' <- force newValue
    trace $ "modifyVar " ++ show name ++ ": " ++ show oldValue ++ " -> " ++ show newValue'
    return (oldValue, newValue')

