{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Data.RDF
import qualified Data.Text as T (Text, splitOn, unpack)
import System.FilePath.Glob (glob)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

db :: IO ()
db = runSqlite "test.db" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    -- delete janeId
    -- deleteWhere [BlogPostAuthorId ==. johnId]

testText :: FilePath
testText = "../gutenberg-meta/rdf-files/cache/epub/10885/pg10885.rdf"

testText2 :: FilePath
testText2 = "../gutenberg-meta/rdf-files/cache/epub/15233/pg15233.rdf" 

parseRDFXML :: String -> IO (Either ParseFailure (RDF TList))
parseRDFXML = parseFile (XmlParser Nothing Nothing)

-- getTitle :: RDF TList ->

-- | Gets a value out of a LNode PlainL in the object of a triple,
-- Where the queryString is the verb.
-- For example: getPlains rdf "dcterms:title" should return the title. 
getPlains :: RDF TList -> T.Text -> [T.Text] 
getPlains rdf queryString = objects where
  triples = getTriples rdf queryString
  objects = map (getTitle . objectOf) triples
  getTitle node = myTitle where (LNode (PlainL myTitle)) = node

getURIs :: RDF TList -> T.Text -> [T.Text]
getURIs rdf verb = objectURIs where
  triples = getTriples rdf verb
  objectURIs = map (getURI . objectOf) triples
  getURI node = objectURI where (UNode objectURI) = node

getTriples :: RDF TList -> T.Text -> Triples
getTriples rdf verb = query rdf Nothing (Just (UNode verb)) Nothing

getTitles :: RDF TList -> [Title]
getTitles rdf = getPlains rdf "dcterms:title"

getTOCs :: RDF TList -> [TOC]
getTOCs rdf = getPlains rdf "dcterms:tableOfContents"

getAuthors :: RDF TList -> [AuthorID]
getAuthors rdf = map (parseAuthor . getURIpath) $ getURIs rdf "dcterms:creator" where
  parseAuthor rawAuthor = read $ T.unpack rawAuthor

-- | Takes a path, like http://some-URL-here.com/path/to/the-thing and returns the-thing
getURIpath :: T.Text -> T.Text
getURIpath uri = last $ T.splitOn "/" uri

getID :: RDF TList -> BookID
getID rdf = bookID where
  triples = query rdf Nothing Nothing (Just (UNode "pgterms:ebook"))
  ids = map (extractID . subjectOf) triples
  extractID node = getURIpath idURI where
    (UNode idURI) = node
  bookID = case ids of
    [_,_] -> error "More than one ID."
    [x] -> read $ T.unpack x
    _ -> error "No IDs or something wrong with the ID."

data Book = Book {id :: BookID,
                  titles :: [Title],
                  authors :: [AuthorID],
                  tocs :: [TOC]} deriving Show

type AuthorID = Int
type Title = T.Text
type BookID = Int
type TOC = T.Text

testFile file = do
  parsed <- parseRDFXML file
  let result = fromEither parsed
      testBook = Book (getID result) (getTitles result) (getAuthors result) (getTOCs result)
  print testBook

rdfFiles :: IO [FilePath]
rdfFiles = glob "../gutenberg-meta/**/*.rdf"

main :: IO ()
main = do
  testFile testText2
