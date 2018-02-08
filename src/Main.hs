{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import           Control.Applicative                  ((<$>))
import           Controllers.Home                     (home, docs, login)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.Environment                   (getEnv)
import           Web.Scotty                           (middleware, scotty)

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" } 

allUsers :: [User]
allUsers = [bob, jenny]

instance ToJSON User
instance FromJSON User

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 80 $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/api/users" $ do
      json allUsers
    get "/api/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers)
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    home >> docs >> login
