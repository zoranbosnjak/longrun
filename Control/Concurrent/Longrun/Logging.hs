-----------------------------------------------------------
-- |
-- Module       : Control.Concurrent.Longrun.Logging
-- Copyright    : (c) Zoran Bošnjak 2016
-- License      : GLPv3
--
-- Functions for long running process handling (Logging).
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

module Control.Concurrent.Longrun.Logging
where

import qualified System.Log.Logger as Log
import System.Log.Handler.Simple (verboseStreamHandler)
import System.IO (Handle, stdout)

-- | Log handle
data LHandle
    = LHandleConsole    -- log to console
    | LHandleSyslog     -- log to syslog
    | LHandle Handle    -- log (append) file handle
    deriving (Show)

instance Eq LHandle where
    LHandleConsole == LHandleConsole = True
    LHandleSyslog == LHandleSyslog = True
    LHandle h1 == LHandle h2 = h1 == h2
    _ == _ = False

initLogging :: IO ()
initLogging = do
    -- TODO: temporary logging
    Log.updateGlobalLogger Log.rootLoggerName
        (Log.setLevel Log.DEBUG . Log.removeHandler)
    console <- verboseStreamHandler stdout Log.DEBUG
    Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler console)

