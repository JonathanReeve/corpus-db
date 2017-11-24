{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base

db :: String
db = "/home/jon/Code/gitenberg-scrape/pg-text-7.db"

getPerson :: (Convertible a SqlValue, IConnection conn) => conn -> a -> IO [[SqlValue]]
getPerson conn person =
  quickQuery' conn "select title, id from meta where author like ?" [toSql person]

main :: IO ()
main = do
    conn <- connectSqlite3 db
    person <- getPerson conn "Dickens, Charles"
    print person
