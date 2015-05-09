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
{-|

  Client library for @pg-harness@ REST service.

  Use the 'createTemporaryDatabase' function to create the database.

-}
module Data.PostgreSQL.Harness.Client
    ( ConnectionInformation(..)
    , createTemporaryDatabase
    , toConnectionString
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Network.HTTP (simpleHTTP, postRequest, getResponseBody)

-- | Connection information to use for connecting to a database.
data ConnectionInformation = ConnectionInformation
    { ciHost :: String
    , ciPort :: String
    , ciDatabaseName :: String
    , ciUser :: String
    , ciPassword :: String
    }

-- | Create temporary database using the given URL to a
-- running @pg-harness@ REST service. Returns the connection
-- information for connecting to the newly created temporary
-- database.
createTemporaryDatabase :: String -> IO ConnectionInformation
createTemporaryDatabase url = do
  rsp <- simpleHTTP (postRequest url)
  body <- getResponseBody rsp
  return $ parse body
  where
    parse :: String -> ConnectionInformation
    parse s =
      let (userPass, (_:hostPortDatabase)) = break ((==) '@') s in
      let (user, (_:password)) = break ((==) ':') userPass in
      let (hostPort, (_:databaseName)) = break ((==) '/') hostPortDatabase in
      let (host, (_:port)) = break ((==) ':') hostPort in
        ConnectionInformation host port databaseName user password

-- | Convert connection information to a @libpq@- or
-- @postgresql-simple@-compatible connection string.
toConnectionString :: ConnectionInformation -> ByteString
toConnectionString (ConnectionInformation host port databaseName user password) =
  B8.pack $
    "host=" ++ host ++ " " ++
    "port=" ++ port ++ " " ++
    "dbname=" ++ databaseName ++ " " ++
    "user=" ++ user ++ " " ++
    "password=" ++ password
