{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Data.Map (fromList)
import Data.Monoid ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Aeson (toJSON)
-- import Control.Applicative ((<$>))
import Controllers.Home (home, docs, login)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (addBase, noDots,
                                             staticPolicy, (>->))
import Web.Scotty

db :: String
db = "/home/jon/Code/gitenberg-scrape/pg-text-7.db"

-- getPerson :: (Convertible String SqlValue, IConnection conn) => conn -> String -> IO [[SqlValue]]
getByAuthor conn person = do
  stmt <- prepare conn "select * from meta where author like ?"
  _ <- execute stmt [toSql person]
  fetchAllRowsAL stmt

-- getByID :: (Convertible String SqlValue) => String -> Maybe [(String, SqlValue)]
getByID conn bookID = do
  stmt <- prepare conn "select * from meta where id = ?"
  _ <- execute stmt [toSql bookID]
  fetchRowAL stmt

sqlToText :: Maybe [(String, SqlValue)] -> Maybe [(String, String)]
sqlToText maybeSqlPairList = case maybeSqlPairList of
  Nothing -> Nothing
  Just sqlPairList -> Just $ map getVal sqlPairList where
    getVal (a, val) = case val of SqlNull -> (a, "NULL")
                                  _ -> (a, fromSql val :: String)

filterOutFields :: Maybe [(String, String)] -> Maybe [(String, String)]
filterOutFields maybeSqlPairList = case maybeSqlPairList of
  Nothing -> Nothing
  Just sqlPairList -> Just $ filter allowed sqlPairList where
    allowed (key, _) = take 3 key `notElem` ["am_", "gr_"]

-- textToJson :: Maybe [(String, String)] -> String
textToJson maybePairList = case maybePairList of
  Nothing -> ""
  Just pairList -> do
    let myMap = fromList pairList
    toJSON myMap

processSql sqlPairList = textToJson $ filterOutFields $ sqlToText sqlPairList

main :: IO ()
main = do
  putStrLn "Starting Server..."
  conn <- connectSqlite3 db
  scotty 8000 $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/api/:id" $ do
      bookID <- param "id"
      sql <- lift $ getByID conn (bookID::String)
      json $ processSql sql
    get "/api/author/:author" $ do
      author <- param "author"
      sql <- lift $ getByAuthor conn (author::String)
      json $ map (processSql . Just) sql
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
