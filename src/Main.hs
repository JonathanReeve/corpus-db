{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.RDF

testText :: FilePath
testText = "../gutenberg-meta/rdf-files/cache/epub/10885/pg10885.rdf"

parseRDFXML :: String -> IO (Either ParseFailure (RDF TList))
parseRDFXML = parseFile (XmlParser Nothing Nothing)
  
main :: IO ()
main = do
  parsed <- parseRDFXML testText
  putStrLn "hello world"
  print parsed
