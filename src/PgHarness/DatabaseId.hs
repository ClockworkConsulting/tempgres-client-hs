module PgHarness.DatabaseId
    ( DatabaseId
    , mkDatabaseId
    , sqlIdentifier
    , unquotedIdentifier
    ) where

-- Newtype to prevent unsafe construction.
newtype DatabaseId = DatabaseId String
    deriving (Show)

-- Create a new database identifier. This implementation is
-- *extremely* conservative in what is accepts in the input
-- string.
mkDatabaseId :: String -> Either String DatabaseId
mkDatabaseId s = do
  -- Since we don't do any quoting or anything we just need to check
  -- if the string obeys all the rules.
  s' <- first letters s
  s'' <- rest (letters ++ digits) s'
  return $ s'' `seq` DatabaseId s
  where
    rest :: [Char] -> String -> Either String String
    rest _       [ ]                       = Right [ ]
    rest choices (c:cs) | c `elem` choices = rest choices cs
    rest _       (c:_)  | otherwise        = invalid c

    first :: [Char] -> String -> Either String String
    first _       [ ]                       = Left "Database name cannot be empty"
    first choices (c:cs) | c `elem` choices = Right cs
    first _       (c:_)  | otherwise        = invalid c

    invalid c = Left $ "Invalid character '" ++ [c] ++ "' in database name '" ++ s ++ "'"

    letters = "abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    digits = "0123456789"

-- Turn database identifier into an SQL identifier for the
-- database. Will include quotes if necessary.
sqlIdentifier :: DatabaseId -> String
sqlIdentifier (DatabaseId s) = s -- Our whitelist ensures that we do not need any quoting.

-- Turn database identifier into a RAW identifier for the
-- database. Will NOT include quotes!
unquotedIdentifier :: DatabaseId -> String
unquotedIdentifier (DatabaseId s) = s
