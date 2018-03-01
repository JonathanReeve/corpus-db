{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Data.List (intersperse)
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
import System.Environment (getEnv)
import Web.Scotty

data Environment = Environment {dbPath :: String, port :: Int}

dev :: Environment
dev = Environment "/home/jon/Code/gitenberg-scrape/pg-text-7.db" 8000

prod :: Environment
prod = Environment "/mnt/vol/pg-text-7.db" 80

mkEnv :: String -> Environment
mkEnv rawEnv = case rawEnv of
  "dev" -> dev
  "prod" -> prod
  _ -> error "Environment must be one of 'prod' (production) or 'dev' (development)."

getByAuthor :: IConnection conn => conn -> String -> IO [[(String, SqlValue)]]
getByAuthor conn person = do
  stmt <- prepare conn "select * from meta where author like ?"
  _ <- execute stmt [toSql person]
  fetchAllRowsAL stmt

getIDsByAuthor :: IConnection conn => conn -> String -> IO [[SqlValue]]
getIDsByAuthor conn person = do
  stmt <- prepare conn "select id from meta where author like ?"
  _ <- execute stmt [toSql person]
  fetchAllRows stmt

getFullText :: IConnection conn => conn -> [SqlValue] -> IO [[(String, SqlValue)]]
getFullText conn ids = do
  let query = "select id, text from text where id in (" ++ intersperse ',' ('?' <$ ids) ++ ")"
  stmt <- prepare conn query
  _ <- execute stmt ids
  fetchAllRowsAL stmt

getByID :: IConnection conn => conn -> String -> IO (Maybe [(String, SqlValue)])
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

--processSql :: Maybe [(String, SqlValue)] -> Data.Aeson.Types.Internal.Value
processSql sqlPairList = textToJson $ filterOutFields $ sqlToText sqlPairList

main :: IO ()
main = do
  putStrLn "Starting server..."
  envRaw <- getEnv "ENV"
  let env = mkEnv envRaw
  conn <- connectSqlite3 (dbPath env)
  scotty (port env) $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/api/id/:id" $ do
      bookID <- param "id"
      sql <- lift $ getByID conn (bookID::String)
      json $ processSql sql
    get "/api/id/:id/fulltext" $ do
      bookID <- param "id"
      sql <- lift $ getFullText conn [toSql (bookID::String)]
      json $ map (processSql . Just) sql
    get "/api/author/:author" $ do
      author <- param "author"
      sql <- lift $ getByAuthor conn (author::String)
      json $ map (processSql . Just) sql
    get "/api/author/:author/fulltext" $ do
      author <- param "author"
      ids <- lift $ getIDsByAuthor conn (author::String)
      sql <- lift $ getFullText conn (map head ids)
      json $ map (processSql . Just) sql
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
