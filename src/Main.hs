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

-- import           Control.Monad (liftM)
import           Control.Monad.IO.Class  (liftIO)
-- import           Database.Esqueleto
import           Database.Persist
import           Database.Persist.Class (Key)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Either                          (fromRight)

type Year = Int
type Title = T.Text
type URI = T.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book
    gutId Int
    UniqueGutId gutId
    title [Title]
    author [AuthorId]
    toc [T.Text] Maybe
    deriving Show
Author
    gutId Int
    UniqueAuthorGutId gutId
    names [T.Text]
    dob Year Maybe
    dod Year Maybe
    webpage URI Maybe
    deriving Show
|]


testText :: FilePath
testText = "../gutenberg-meta/rdf-files/cache/epub/10885/pg10885.rdf"

testText2 :: FilePath
testText2 = "../gutenberg-meta/rdf-files/cache/epub/15233/pg15233.rdf"

-- This one has two authors. 
testText3 :: FilePath
testText3 = "../gutenberg-meta/rdf-files/cache/epub/1423/pg1423.rdf"

parseRDFXML :: String -> IO (Either ParseFailure (RDF TList))
parseRDFXML = parseFile (XmlParser Nothing Nothing)

-- Try this again using a different representation. 
parseRDFXML' :: String -> IO (Either ParseFailure (RDF AdjHashMap))
parseRDFXML' = parseFile (XmlParser Nothing Nothing)

parseTurtle :: String -> IO (Either ParseFailure (RDF TList))
parseTurtle = parseFile (TurtleParser Nothing Nothing)

parseNTriples :: String -> IO (Either ParseFailure (RDF TList))
parseNTriples = parseFile (NTriplesParser)

-- | Gets a value out of a LNode PlainL in the object of a triple,
-- Where the queryString is the verb.
-- For example: getPlains rdf "dcterms:title" should return the title.
getPlains :: RDF TList -> T.Text -> [T.Text] 
getPlains rdf queryString = objects where
  triples = getTriples rdf queryString
  objects = map (getTitle . objectOf) triples
  getTitle node = myTitle where (LNode (PlainL myTitle)) = node

-- | Gets a value out of a LNode PlainL in the object of a triple,
--   given some triples.
--   For example: getPlains triples "dcterms:title" should return the title.
getPlainsFromTriples :: Triples -> T.Text -> [T.Text]
getPlainsFromTriples triples queryString = [name | (LNode (PlainL name)) <- nameObjects] where
  nameTriples = [nameTriple | nameTriple <- triples, predicateOf nameTriple == UNode queryString]
  nameObjects = map objectOf nameTriples

-- | Gets an integer value out of a LNode TypedL in the object of a triple,
--   given some triples.
--   For example: getIntrFrom triples "pgterms:birthdate" should return the author birth date. 
getIntsFromTriples :: Triples -> T.Text -> [Int]
getIntsFromTriples triples queryString =
  map parseInt [name | (LNode (TypedL name "http://www.w3.org/2001/XMLSchema#integer")) <- nameObjects] where
  nameTriples = [nameTriple | nameTriple <- triples, predicateOf nameTriple == UNode queryString]
  nameObjects = map objectOf nameTriples

getURIs :: RDF TList -> T.Text -> [T.Text]
getURIs rdf verb = map getURI nodes where
  nodes = getNodes rdf verb
  getURI node = objectURI where (UNode objectURI) = node

getURIsFromTriples :: Triples -> T.Text -> [T.Text]
getURIsFromTriples triples verb = map getURI nodes where
  nodes = map objectOf [t | t <- triples, predicateOf t == UNode verb]
  getURI node = objectURI where (UNode objectURI) = node

getNodes :: RDF TList -> T.Text -> [Node]
getNodes rdf verb = map objectOf triples where
  triples = getTriples rdf verb

getTriples :: RDF TList -> T.Text -> Triples
getTriples rdf verb = query rdf Nothing (Just (UNode verb)) Nothing

getTitles :: RDF TList -> [Title]
getTitles rdf = getPlains rdf "dcterms:title"

getTOCs :: RDF TList -> Maybe [T.Text]
getTOCs rdf = let parsed = getPlains rdf "dcterms:tableOfContents" in
  case parsed of
    [] -> Nothing
    _ -> Just parsed

parseInt :: T.Text -> Int
parseInt str = case decimal str of
  (Right num) -> fst num
  (Left _)    -> error "Couldn't parse the integer."

-- | Returns a triples set about each author.
getAuthorsInfo :: RDF TList -> [Triples]
getAuthorsInfo rdf = [query rdf nodes Nothing Nothing |
                     nodes <- map Just (getNodes rdf "dcterms:creator")]

mkAuthor :: Triples -> Author
mkAuthor ai = Author gutID name dob dod webpage where
  gutID = getAuthorGutId ai
  name = getAuthorNames ai
  dob = getAuthorDOB ai
  dod = getAuthorDOD ai
  webpage = getAuthorWebpage ai

-- | Just get the author Gutenberg ID.
getAuthorGutIds :: RDF TList -> [Int]
getAuthorGutIds rdf = map (parseInt . getURIpath) $ getURIs rdf "dcterms:creator"

-- | Just get the author Gutenberg ID from a single author's triples.
getAuthorGutId :: Triples -> Int
getAuthorGutId triples = parseInt $ getURIpath agent where
  (UNode agent) = subjectOf $ head triples

-- | Takes the triples of info related to a single author,
--   and returns a list of names for that author.
getAuthorNames :: Triples -> [T.Text]
getAuthorNames authorInfo = getPlainsFromTriples authorInfo "pgterms:name"

-- | Takes the triples of info related to a single author,
--   and returns the author year of birth.
getAuthorDOB :: Triples -> Maybe Int
getAuthorDOB authorInfo = case getIntsFromTriples authorInfo "pgterms:birthdate" of
  [] -> Nothing
  result -> Just $ head result

-- | Takes the triples of info related to a single author,
--   and returns the author year of death.
getAuthorDOD :: Triples -> Maybe Int
getAuthorDOD authorInfo = case getIntsFromTriples authorInfo "pgterms:deathdate" of
  [] -> Nothing
  result -> Just $ head result

-- | Takes the triples of info related to a single author,
--   and returns the author's webpage, if it exists. 
getAuthorWebpage :: Triples -> Maybe URI
getAuthorWebpage authorInfo = case getURIsFromTriples authorInfo "pgterms:webpage" of
  [] -> Nothing
  result  -> Just $ head result

-- | Takes a path, like http://some-URL-here.com/path/to/the-thing and returns the-thing
getURIpath :: T.Text -> T.Text
getURIpath uri = last $ T.splitOn "/" uri

getID :: RDF TList -> Int
getID rdf = bookID where
  triples = query rdf Nothing Nothing (Just (UNode "pgterms:ebook"))
  ids = map (extractID . subjectOf) triples
  extractID node = getURIpath idURI where
    (UNode idURI) = node
  bookID = case ids of
    [_,_] -> error "More than one ID."
    [x] -> read $ T.unpack x
    _ -> error "No IDs or something wrong with the ID."

-- rdfFiles :: IO [FilePath]
rdfFiles = glob "../gutenberg-meta/**/*.rdf"

parseMeta :: FilePath -> IO ()
parseMeta file = do
  parsed <- parseRDFXML file
  let result = fromEither parsed
      authorInfo = getAuthorsInfo result
      authors = map mkAuthor authorInfo

  -- print authors
  runSqlite "test.db" $ do
    runMigration migrateAll

    -- FIXME: This won't work if there's an author that's already in the database.

    authorEntities <- mapM (\r -> upsert r []) authors

    let authorKeys = map entityKey authorEntities

    testBookIdEither <- upsertBy $ Book (getID result) (getTitles result) authorKeys (getTOCs result)

    testBookResult <- selectList [BookId ==. testBookId] [LimitTo 1]

    liftIO $ print testBookResult

main :: IO ()
main = parseMeta testText2
