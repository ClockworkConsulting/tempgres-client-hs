module Main ( main ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, withAsync, waitCatch)
import           Control.Exception (bracket)
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Data.String (fromString)
import qualified Data.Text.Lazy as TL
import           Database.PostgreSQL.Simple (Connection, ConnectInfo(..), Only(..))
import qualified Database.PostgreSQL.Simple as P
import           System.IO (stderr, hPutStrLn)
import           System.Random (randomRIO)
import           Web.Scotty (scotty, post, text, ScottyM, raise)
import           Paths_pg_harness (getDataFileName)
import           PgHarness.Mutex
import           PgHarness.Configuration
import           PgHarness.DatabaseId

-- Perform an operation with an open connection to the database.
-- The connection will automatically be closed after the operation
-- completes, regardless of whether it completes successfully or
-- not (e.g. if there's an exception).
withConnection :: Configuration -> (Connection -> IO a) -> IO a
withConnection configuration action = bracket (P.connect connectInfo) P.close action
  where
    connectInfo = ConnectInfo
      { connectHost = cfgHost configuration
      , connectPort = cfgPort configuration
      , connectUser = cfgUser configuration
      , connectPassword = cfgPassword configuration
      , connectDatabase = unquotedIdentifier (cfgDatabase configuration)
      }

-- Create a random valid PostgreSQL identifier.
mkRandomIdent :: IO String
mkRandomIdent = do
  h <- chooseElement letters
  t <- sequence $ replicate 32 $ chooseElement lettersAndDigits
  return (h:t)
  where
    letters = "abcdefhijklmnopqrstuvwxyz"
    digits = "0123456789"
    lettersAndDigits = letters ++ digits
    chooseElement t = do
      i <- randomRIO (0, length t - 1)
      return $ t !! i

-- Create temporary database ID.
mkTemporaryDatabaseId :: IO (Either String DatabaseId)
mkTemporaryDatabaseId = fmap mkDatabaseId $ fmap ("temp_" ++) mkRandomIdent

-- Create temporary database and return its name.
createTemporaryDatabase :: Configuration -> DatabaseId -> IO ()
createTemporaryDatabase configuration databaseId = do
  -- Connect to the administrative database.
  withConnection configuration $ \connection -> do
    -- Create the temporary database.
    void $ P.execute_ connection $ createSql
    putStrLn $ "Created temporary database: " ++ sqlDatabaseId
    -- Spawn a thread to kill the database after a certain delay.
    void $ async $ do
      threadDelay $ (cfgDurationSeconds configuration) * 1000000
      dropDatabase configuration databaseId
  where
    createSql = fromString $
      "CREATE DATABASE " ++ sqlDatabaseId ++
      "  WITH TEMPLATE " ++ (sqlIdentifier $ cfgTemplateDatabase configuration) ++
      "          OWNER \"" ++ (cfgTestUser configuration) ++ "\""

    sqlDatabaseId = sqlIdentifier databaseId

dropDatabase :: Configuration -> DatabaseId -> IO ()
dropDatabase configuration databaseId =
  -- Since this is running in a separate thread we need explicit
  -- exception handling to avoid silent exceptions.
  withAsync doDropDatabase waitCatch >>= \case
    Left exc -> hPutStrLn stderr $ "Exception occurred while creating temporary database: " ++ show exc
    Right _  -> return ()
  where
    doDropDatabase = withConnection configuration $ \connection -> do
      -- We need to block new connections to the temporary database; otherwise
      -- a reconnect during the destruction could foil our attempt to drop.
      void $ P.execute_ connection blockConnectionsSql
      -- Now we can kill the backends, i.e. terminate all the connections. No
      -- new connections can be created because of the previous block.
      P.forEach_ connection terminateConnectionsSql $ \(Only (_::Bool)) -> return ()
      -- Finally, we can drop.
      void $ P.execute_ connection dropSql
      -- Logging
      putStrLn $ "Dropped temporary database: " ++ sqlDatabaseId

    dropSql = fromString $
        "DROP DATABASE " ++ sqlDatabaseId

    blockConnectionsSql = fromString $
        "UPDATE pg_database \
        \   SET datallowconn = FALSE \
        \ WHERE datname = '" ++ sqlDatabaseId ++ "'"

    terminateConnectionsSql = fromString $
        "SELECT pg_terminate_backend(pid) \
        \  FROM pg_stat_activity \
        \ WHERE pid <> pg_backend_pid() \
        \   AND datname = '" ++ sqlDatabaseId ++ "'"

    sqlDatabaseId = sqlIdentifier databaseId

-- REST interface
routes :: Configuration -> ScottyM ()
routes configuration = do
  -- Mutex to prevent multiple "create" requests from being
  -- processed simultaneously; PostgreSQL cannot handle
  -- "cloning" a template concurrently.
  mutex <- lift $ mkMutex
  -- Add all the routes.
  post "/" $ do
    -- Generate a name for temporary database.
    lift mkTemporaryDatabaseId >>= \case
      Left err ->
        raise $ TL.pack err
      Right databaseId -> do
        -- Create the temporary database.
        lift $ mutex $ createTemporaryDatabase configuration databaseId
        -- Return a string with the username/password and database name.
        text $ TL.pack $
          cfgTestUser configuration ++ ":" ++ cfgTestPassword configuration ++
          "@" ++
          cfgHost configuration ++ ":" ++ (show $ cfgPort configuration) ++
          "/" ++
          (unquotedIdentifier databaseId)

main :: IO ()
main = do
  -- Load configuration from file.
  getDataFileName "pg-harness.ini" >>= loadConfiguration >>= \case
    Left msg -> hPutStrLn stderr msg
    Right configuration -> do
      -- Start the web serving thread
      putStrLn $ "Starting with configuration: " ++ show configuration
      scotty (cfgListenPort configuration) $ routes configuration
