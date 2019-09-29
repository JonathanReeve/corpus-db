{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Auth as GH
import Data.List
import Data.Maybe
import Data.Vector as V (head)

import Secrets (username, password)

main :: IO ()
main = do
  putStrLn "hello world"
  let auth = GH.BasicAuth username password
  testRepo <- GH.repository' (Just auth) "GITenberg" "Pride-and-Prejudice_1342"
  case testRepo of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       -- (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo repos
       (Right repos) -> print repos
