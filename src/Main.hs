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
import Controllers.Home (home, docs, login)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (addBase, noDots,
                                             staticPolicy, (>->))
import System.Environment (getEnv)
import Web.Scotty

-- Functions for setting up the environment.
-- The user sets the ENV environment variable to either "dev" or "prod,"
-- And then we invoke the appropriate settings here.

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

-- Functions for getting certain types of data from the database.

getByAuthor :: IConnection conn => conn -> String -> IO [[(String, SqlValue)]]
getByAuthor conn person = do
  stmt <- prepare conn "select * from meta where author like ?"
  _ <- execute stmt [toSql person]
  fetchAllRowsAL stmt

getBySubject :: IConnection conn => conn -> String -> IO [[(String, SqlValue)]]
getBySubject conn subject = do
  stmt <- prepare conn "select * from meta where instr(lcsh, ?) > 0"
  _ <- execute stmt [toSql subject]
  fetchAllRowsAL stmt

-- TODO: finish this function
getAllSubjects :: IConnection conn => conn -> IO [[SqlValue]]
getAllSubjects conn = do
  stmt <- prepare conn "select lcsh from meta"
  _ <- execute stmt []
  fetchAllRows stmt

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

-- Utility functions for converting to/from JSON, strings, etc. 

sqlToText :: [(String, SqlValue)] -> [(String, String)]
sqlToText = map getVal where
  getVal (a, val) = case val of SqlNull -> (a, "NULL")
                                _       -> (a, fromSql val :: String)

filterOutFields :: [(String, String)] -> [(String, String)]
filterOutFields = filter allowed where
  allowed (key, _) = take 3 key `notElem` ["am_", "gr_"]

-- TODO: figure out how to get type declaration for this
textToJson = maybe "" (toJSON . fromList)

-- TODO: figure out how to get type declaration for this
processSql = textToJson . fmap (filterOutFields . sqlToText)

main :: IO ()
main = do
  putStrLn "Starting server..."
  envRaw <- getEnv "ENV"
  let env = mkEnv envRaw
  conn <- connectSqlite3 (dbPath env)
  scotty (port env) $ do
    -- Easter egg! And to make sure everything's working correctly. 
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
    get "/api/subject/:subject" $ do
      subject <- param "subject"
      sql <- lift $ getBySubject conn (subject::String)
      json $ map (processSql . Just) sql
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
