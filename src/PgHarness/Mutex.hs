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
