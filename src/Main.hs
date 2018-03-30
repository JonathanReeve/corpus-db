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
import Data.Text.Read (decimal)
import System.FilePath.Glob (glob)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

-- data Book = Book {id :: BookID,
--                   titles :: [Title],
--                   authors :: [AuthorID],
--                   tocs :: [TOC]} deriving Show

-- type AuthorID = Int
-- type Title = T.Text
-- type BookID = Int
-- type TOC = T.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book
    gutId Int
    title [T.Text]
    toc [T.Text] Maybe
    deriving Show
Author
    gutId Int
    name String
|]


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
getURIs rdf verb = map getURI nodes where
  nodes = getNodes rdf verb
  getURI node = objectURI where (UNode objectURI) = node

-- getNodes :: RDF TList -> [Node]
getNodes rdf verb = map objectOf triples where
  triples = getTriples rdf verb

getTriples :: RDF TList -> T.Text -> Triples
getTriples rdf verb = query rdf Nothing (Just (UNode verb)) Nothing

-- getTitles :: RDF TList -> [BookTitle]
getTitles rdf = getPlains rdf "dcterms:title"

getTOCs :: RDF TList -> Maybe [T.Text]
getTOCs rdf = let parsed = getPlains rdf "dcterms:tableOfContents" in
  case parsed of
    [] -> Nothing
    _ -> Just parsed

-- getAuthors :: RDF TList -> [BookAuthorID]
-- getAuthors rdf = map (parseAuthor . getURIpath) $ getURIs rdf "dcterms:creator" where
--   parseAuthor rawAuthor = read $ T.unpack rawAuthor

-- | Just get the author Gutenberg ID. 
getAuthors rdf = map (parseAuthor . getURIpath) $ getURIs rdf "dcterms:creator" where
  parseAuthor rawAuthor = case decimal rawAuthor of
    (Right num) -> fst num
    (Left _)    -> error "Couldn't parse the author ID." 

-- | Returns a triples set about each author.
getAuthorsInfo :: RDF TList -> [Triples]
getAuthorsInfo rdf = [query rdf nodes Nothing Nothing |
                     nodes <- map Just (getNodes rdf "dcterms:creator")]

-- getAuthorName :: Triples -> T.Text
getAuthorName authorInfo = [name | (LNode (PlainL name)) <- nameObjects] where
  nameTriples = [triple | triple <- authorInfo, predicateOf triple == (UNode "pgterms:name")]
  nameObjects = map objectOf nameTriples

getAuthorsNames rdf = map getAuthorName (getAuthorsInfo rdf)

-- | Takes a path, like http://some-URL-here.com/path/to/the-thing and returns the-thing
getURIpath :: T.Text -> T.Text
getURIpath uri = last $ T.splitOn "/" uri

-- getID :: RDF TList -> BookGutId
getID rdf = bookID where
  triples = query rdf Nothing Nothing (Just (UNode "pgterms:ebook"))
  ids = map (extractID . subjectOf) triples
  extractID node = getURIpath idURI where
    (UNode idURI) = node
  bookID = case ids of
    [_,_] -> error "More than one ID."
    [x] -> read $ T.unpack x
    _ -> error "No IDs or something wrong with the ID."

rdfFiles :: IO [FilePath]
rdfFiles = glob "../gutenberg-meta/**/*.rdf"

main :: IO ()
main = do
  parsed <- parseRDFXML testText2
  let result = fromEither parsed
      testBook = Book (getID result) (getTitles result) (getTOCs result)
  print testBook
  runSqlite "test.db" $ do
    runMigration migrateAll

    -- TODO: insert author first
    
    testBookId <- insert $ testBook

    testBookResult <- selectList [BookId ==. testBookId] [LimitTo 1]
    liftIO $ print testBookResult
