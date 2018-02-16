{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Monoid ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Control.Applicative                  ((<$>))
import Controllers.Home                     (home, docs, login)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (addBase, noDots,
                                             staticPolicy, (>->))
import Web.Scotty

db :: String
db = "/home/jon/Code/gitenberg-scrape/pg-text-7.db"

getPerson :: (Convertible a SqlValue, IConnection conn) => conn -> a -> IO [[SqlValue]]
getPerson conn person =
  quickQuery' conn "select title, id from meta where author like ?" [toSql person]

getByID :: (Convertible String SqlValue, IConnection conn) =>
           conn -> String -> IO (Maybe [(String, SqlValue)])
getByID conn bookID = do
  stmt <- prepare conn "select * from meta where id = ?"
  _ <- execute stmt [toSql bookID]
  fetchRowAL stmt

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 8000 $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/api/id" $ do
      conn <- connectSqlite3 db
      meta <- getByID conn "9.0"
      json meta
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
