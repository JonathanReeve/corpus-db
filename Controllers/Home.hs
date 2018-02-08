{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    , docs
    , login
    ) where

import Text.Blaze.Html5 (Html)
import Views.Home (homeView, apiDocsView)
import Web.Scotty (ScottyM, get, html)

home :: Html -> ScottyM ()
home contents = get "/" (homeView contents)

docs :: ScottyM ()
docs = get "/docs" apiDocsView

login :: ScottyM ()
login = get "/login" $ html "login"
