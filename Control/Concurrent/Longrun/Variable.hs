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
( IsVar(..)
, GettableVar(..)
, GetEnd(..)
, getEnd
, Var(Var)
, newVar
, newVarBind
, setVar
, modifyVar
, modifyVar_
, onChangeVar
) where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Longrun.Base

data Var a = Var (STM.TVar a)

newtype GetEnd a = GetEnd (Var a)

getEnd :: Var a -> GetEnd a
getEnd = GetEnd

-- | Create new variable.
newVar :: a -> Process (Var a)
newVar val = Var <$> (liftIO $ STM.newTVarIO val)

-- | Bind existing TVar to a variable
newVarBind :: (STM.TVar a) -> Process (Var a)
newVarBind = return . Var

class IsVar v a where
    -- | Get STM.TVar out of the structure.
    vVar :: v a -> STM.TVar a

instance IsVar Var a where
    vVar (Var v) = v

instance IsVar GetEnd a where
    vVar (GetEnd (Var v)) = v

class GettableVar v a where
    -- | Get variable content.
    getVar :: v a -> Process a

-- | Get variable content from Var.
instance GettableVar Var a where
    getVar (Var var) = liftIO $ STM.atomically $ STM.readTVar var

-- | Get variable content from GetEnd.
instance GettableVar GetEnd a where
    getVar (GetEnd v) = getVar v

-- | Set variable content.
setVar :: Var a -> a -> Process ()
setVar (Var var) val = liftIO $ STM.atomically $ STM.writeTVar var val

-- | Modify variable content.
modifyVar :: Var a -> (a -> a) -> Process (a,a)
modifyVar (Var var) f = liftIO $ STM.atomically $ do
    a <- STM.readTVar var
    STM.modifyTVar var f
    b <- STM.readTVar var
    return (a,b)

-- | Modify variable content, ignore values.
modifyVar_ :: Var a -> (a -> a) -> Process ()
modifyVar_ var f = modifyVar var f >> return ()

-- | Run action on each variable change.
onChangeVar :: (IsVar v t, Eq t)
    => t -> v t -> (t -> t -> Process ()) -> Process ()
onChangeVar initial var action = loop initial where
    loop x = do
        y <- liftIO $ STM.atomically $ do
            y <- STM.readTVar (vVar var)
            case y == x of
                True -> STM.retry
                False -> return y
        action x y
        loop y

