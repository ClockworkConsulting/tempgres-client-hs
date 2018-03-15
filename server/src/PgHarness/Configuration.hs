{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
module PgHarness.Configuration
    ( Configuration(..)
    , loadConfiguration
    ) where

import           Data.Ini (lookupValue, readIniFile, readValue)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Data.Word (Word16)
import           PgHarness.DatabaseId (mkDatabaseId, DatabaseId)

-- Configuration for the application
data Configuration = Configuration
    { cfgListenPort :: Int               -- Port to listen to for requests
    , cfgListenHost :: String            -- Host/interface to listen to for requests
    , cfgUser :: String                  -- Administrator user's user name
    , cfgPassword :: String              -- Administrator user's password
    , cfgHost :: String                  -- PostgreSQL host
    , cfgPort :: Word16                  -- PostgreSQL port
    , cfgDatabase :: DatabaseId          -- Database to connect to while creating temporary database
    , cfgTestUser :: String              -- User to use as owner for the temporary database
    , cfgTestPassword :: String          -- Password to send in reply
    , cfgTemplateDatabase :: DatabaseId  -- Database to use as a template for the temporary database
    , cfgDurationSeconds :: Int          -- Duration of temporary databases
    } deriving (Show)

loadConfiguration :: FilePath -> IO (Either String Configuration)
loadConfiguration iniFilePath = do
  readIniFile iniFilePath >>= \case
    Left err -> return $ Left err
    Right ini -> return $ do
      listenPort       <- readValue      rest       "listenPort" decimal ini
      listenHost       <- lookupValue    rest       "listenHost" ini
      user             <- lookupValue    postgresql "user" ini
      password         <- lookupValue    postgresql "password" ini
      database         <- lookupDatabase postgresql "database" ini
      host             <- lookupValue    postgresql "host" ini
      port             <- readValue      postgresql "port" decimal ini
      testUser         <- lookupValue    postgresql "testUser" ini
      testPassword     <- lookupValue    postgresql "testPassword" ini
      templateDatabase <- lookupDatabase postgresql "templateDatabase" ini
      durationSeconds  <- readValue      postgresql "durationSeconds" decimal ini
      return $ Configuration
             { cfgListenPort = listenPort
             , cfgListenHost = T.unpack listenHost
             , cfgUser = T.unpack user
             , cfgPassword = T.unpack password
             , cfgDatabase = database
             , cfgHost = T.unpack host
             , cfgPort = port
             , cfgTestUser = T.unpack testUser
             , cfgTestPassword = T.unpack testPassword
             , cfgTemplateDatabase = templateDatabase
             , cfgDurationSeconds = durationSeconds
             }
  where
    -- Read a database name from INI file
    lookupDatabase section key ini =
        (fmap T.unpack $ lookupValue section key ini) >>= mkDatabaseId

    -- Sections in the INI file
    postgresql = "postgresql"
    rest = "rest"
