{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Data.Map (fromList)
import Data.Monoid ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base
import Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON, encode)
import Data.Maybe
import GHC.Generics
import Control.Applicative                  ((<$>))
import Controllers.Home                     (home, docs, login)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (addBase, noDots,
                                             staticPolicy, (>->))
import Web.Scotty

db :: String
db = "/home/jon/Code/gitenberg-scrape/pg-text-7.db"

-- getPerson :: (Convertible a SqlValue, IConnection conn) => conn -> a -> IO [[SqlValue]]
getPerson conn person =
  quickQuery' conn "select title, id from meta where author like ?" [toSql person]

-- getByID :: (Convertible String SqlValue, IConnection conn) =>
--            conn -> String -> IO (Maybe [(String, SqlValue)])
getByID bookID = do
  conn <- connectSqlite3 db
  stmt <- prepare conn "select * from meta where id = ?"
  _ <- execute stmt [toSql bookID]
  fetchRowAL stmt

sqlToText :: Maybe [(String, SqlValue)] -> Maybe [(String, String)]
sqlToText sqlPairList = case sqlPairList of
  Nothing -> Nothing
  Just sqlPairList -> Just $ map getVal sqlPairList where
    getVal (a, val) = case val of SqlNull -> (a, "NULL")
                                  otherwise -> (a, fromSql val :: String)

-- textToJson :: Maybe [(String, String)] -> String
textToJson pairList = case pairList of
  Nothing -> ""
  Just pairList -> do
    let myMap = fromList pairList
    toJSON myMap

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 8000 $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/api/id" $ do
      sql <- getByID "9.0"
      text <- sqlToText sql
      meta <- textToJson text
      json meta
      -- meta <- getByID "9.0"
      -- text $ lift meta
      -- json meta
      -- text "hello"
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
