{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    , docs
    , login
    ) where

import Views.Home (homeView, apiDocsView)
import Web.Scotty (ScottyM, get, html)

home :: ScottyM ()
home = get "/" homeView

docs :: ScottyM ()
docs = get "/docs" apiDocsView

login :: ScottyM ()
login = get "/login" $ html "login"
