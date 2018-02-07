{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView, apiDocsView) where

import Client.CSS                  (layoutCss)
import Data.Monoid                 (mempty)
import Data.Text.Lazy              (toStrict)
import Prelude                     hiding (div, head, id)
import Text.Blaze.Html             (Html, toHtml)
import Text.Blaze.Html5            (Html, a, body, button,
                                    dataAttribute, div, docTypeHtml,
                                    form, h1, h2, head, input, li,
                                    link, meta, p, script, style,
                                    title, ul, (!))
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_)
import Views.Utils                 (blaze, pet)
import Web.Scotty                  (ActionM)

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
           pet "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
           pet "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
           pet "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
           pet "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
           head $ do
             title t
             meta ! charset "utf-8"
             meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
             meta ! name "description" ! content "Inspire Text"
             meta ! name "viewport" ! content "width=device-width"
             link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
             link ! href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
             style $ pet $ toStrict layoutCss
           body $ do
             navBar >> b
             script ! src "https://code.jquery.com/jquery-3.2.1.slim.min.js" $ mempty
             script ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" $ mempty
             script ! src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" $ mempty

homeView :: ActionM ()
homeView = blaze $ layout "home" $ do
             div ! class_ "container" $ do
               div ! class_ "jumbotron" $ do
                 h1 "Corpus DB"
                 p "Welcome to the Corpus-DB Project, a textual corpus database for the digital humanities."
                 p $ do a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "http://github.com/JonathanReeve/corpus-db" $ "GitHub"

apiDocsView :: ActionM ()
apiDocsView = blaze $ layout "API Docs" $ do
             div ! class_ "container" $ do
               div ! class_ "jumbotron" $ do
                 h1 "Corpus DB REST API Documentation"
                 p "Welcome to the Corpus-DB Project, a textual corpus database for the digital humanities."
                 p $ do a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "http://github.com/JonathanReeve/corpus-db" $ "GitHub"

navBar :: Html
navBar = div ! class_ "navbar navbar-default navbar-static-top" $ div ! class_ "container" $ do
           div ! class_ "navbar-header" $ do
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
               a ! class_ "navbar-brand" ! href "#" $ "λ"
           div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $ do
             li ! class_ "nav-item active" $ a ! href "/" $ "Home"
             li ! class_ "nav-item" $ a ! href "docs" $ "API"
             li ! class_ "nav-item" $ a ! href "#contact" $ "Contact"
