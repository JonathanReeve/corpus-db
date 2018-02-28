{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView, apiDocsView) where

import Client.CSS                  (layoutCss)
import Data.Monoid                 (mempty)
import Data.Text.Lazy              (toStrict)
import Prelude                     hiding (div, head, id)
import Text.Blaze.Html             (Html, toHtml)
import Text.Blaze.Html5            (Html, a, body, button, code,
                                    dataAttribute, div, docTypeHtml, img,
                                    form, h1, h2, h3, head, input, li,
                                    link, meta, p, script, section, style,
                                    title, ul, (!))
import Text.Blaze.Html5.Attributes (alt, charset, class_, content, href,
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
               div ! class_ "main" $ do
                 p $ do
                   "Corpus-DB is a textual corpus database for the digital humanities. "
                   "This project aggregates public domain texts, enhances their metadata from sources like Wikipedia, "
                   "and makes those texts available according to that metadata. "
                   "This makes it easy to download subcorpora like:"
                 ul $ do
                   li "Bildungsromans"
                   li "Dickens novels"
                   li "Poetry published in the 1880s"
                   li "Novels set in London"
                 p "Corpus-DB has several components:"
                 ul $ do
                   li "Scripts for aggregating metadata, written in Python"
                   li "The database, currently a few SQLite databases"
                   li "A REST API for querying the database, currently in progress"
                   li "Analytic experiments, mostly in Python"
                 p $ do
                   "Read more about the database "
                   a ! href "http://jonreeve.com/2017/06/project-gutenberg-the-database/"
                     $ "at this introductory blog post. "
                   "Scripts used to generate the database are in the "
                   a ! href "https://github.com/JonathanReeve/gitenberg-experiments" $ "gitenberg-experiments repo. "
                   "Some usage examples may be found in the "
                   a ! href "https://github.com/JonathanReeve/corpus-db/tree/master/examples" $ "examples directory"
                   " on GitHub. "
                 h1 ! id "contributing" $ "Contributing"
                 p $ do
                   "I could use some help with this, especially if you know Python or Haskell, "
                   "have library or bibliography experience, or simply like books. "
                   "Get in touch in "
                   a ! href "https://gitter.im/corpus-db/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge" $ "the chat room,"
                   " or "
                   a ! href "mailto:jon.reeve@gmail.com" $ "contact me via email. "

apiDocsView :: ActionM ()
apiDocsView = blaze $ layout "API Docs" $ do
             div ! class_ "container" $ do
               div ! class_ "jumbotron" $ do
                 h1 "Corpus DB REST API Documentation"
                 p "Welcome to the Corpus-DB Project, a textual corpus database for the digital humanities."
                 p $ do a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "http://github.com/JonathanReeve/corpus-db" $ "GitHub"
             div ! class_ "container" $ do
               section ! class_ "item" $ do 
                 h2 "Get all the metadata for a certain book, by ID." 
                 p "Handles Project Gutenberg IDs, for now. NB: Each id must be floating point, for the moment, so id 9 should be 9.0." 
                 code "http://corpus-db.org/api/id/<pg-id>"
                 p "Example: get metadata for book with ID 9" 
                 code "http://corpus-db.org/api/id/9.0"
               section ! class_ "item" $ do 
                 h2 "Get all the metadata for all books by a certain author." 
                 p "Handles Project Gutenberg authors, for now. Write name in the form Last, First."  
                 code "http://corpus-db.org/api/author/<Last, First>"
                 p "Example: get metadata for all books by Jane Austen." 
                 code "http://corpus-db.org/api/author/Austen, Jane"
               section ! class_ "item" $ do 
                 h2 "Get the full text for all books by a certain author." 
                 p "Handles Project Gutenberg authors, for now. Write name in the form Last, First."  
                 code "http://corpus-db.org/api/author/<Last, First>/fulltext"
                 p "Example: get full text for all books by Jane Austen." 
                 code "http://corpus-db.org/api/author/Austen, Jane/fulltext"

navBar :: Html
navBar = div ! class_ "navbar navbar-default navbar-static-top" $ div ! class_ "container" $ do
           div ! class_ "navbar-header" $ do
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
               a ! class_ "navbar-toggler-icon" ! href "#" $ "λ"
           div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav" $ do
             li ! class_ "nav-item active" $ a ! href "/" $ "Home"
             li ! class_ "nav-item" $ a ! href "docs" $ "API"
             li ! class_ "nav-item" $ a ! href "#contact" $ "Contact"
