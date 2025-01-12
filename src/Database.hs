{-# LANGUAGE OverloadedStrings #-}
module Database where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (try)
import System.FilePath ((</>))

data DbResult = DbResult 
    { resultValue :: T.Text
    } deriving (Show)

instance FromRow DbResult where
    fromRow = DbResult <$> field

queryDb :: Connection -> T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
queryDb conn tableName paramName paramValue = do
    let queryStr = Query $ "SELECT eplan_value FROM " <> tableName <> 
                          " WHERE xml_param_name = ? AND xml_value = ?"
    result <- try $ query conn queryStr (paramName, paramValue) :: IO (Either SQLError [DbResult])
    case result of
        Left err -> do
            putStrLn $ "Database error: " ++ show err
            return Nothing
        Right [] -> return $ Just "nope"  -- Matching Python's behavior
        Right (x:_) -> return $ Just $ resultValue x

checkMechanicalParam :: Connection -> T.Text -> IO Bool
checkMechanicalParam conn paramName = do
    let queryStr = Query "SELECT xml_name FROM olcparam WHERE m_e = 'm' AND xml_name = ?"
    result <- try $ query conn queryStr (Only paramName) :: IO (Either SQLError [DbResult])
    case result of
        Left _ -> return False
        Right [] -> return False
        Right _ -> return True

checkElectricalParam :: Connection -> T.Text -> IO Bool
checkElectricalParam conn paramName = do
    let queryStr = Query "SELECT xml_name FROM olcparam WHERE m_e = 'e' AND xml_name = ?"
    result <- try $ query conn queryStr (Only paramName) :: IO (Either SQLError [DbResult])
    case result of
        Left _ -> return False
        Right [] -> return False
        Right _ -> return True
