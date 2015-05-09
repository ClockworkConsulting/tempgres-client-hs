{-
    pg-harness, REST service for creating temporary PostgreSQL databases.
    Copyright (C) 2014, 2015 Bardur Arantsson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module PgHarness.Mutex ( mkMutex ) where

import Control.Exception (bracket_)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)

-- Creates a mutex based on semaphores. The mutex takes the form a
-- function which can be called with an IO action to perform that
-- action in a critical section.
mkMutex :: IO (IO a -> IO a)
mkMutex = do
  mutex <- newQSem 1
  return $ \a -> bracket_ (waitQSem mutex) (signalQSem mutex) a
