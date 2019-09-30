{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    , docs
    , examples
    , login
    ) where

import Views.Home (homeView, apiDocsView, examplesView)
import Web.Scotty (ScottyM, get, html)

home :: ScottyM ()
home = get "/" homeView

docs :: ScottyM ()
docs = get "/docs" apiDocsView

examples :: ScottyM ()
examples = get "/examples" examplesView

login :: ScottyM ()
login = get "/login" $ html "login"
