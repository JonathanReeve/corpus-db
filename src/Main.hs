{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.XML.Light

testText = "../gutenberg-meta/rdf-files/cache/epub/10885/pg10885.rdf"

rdfqn = QName "RDF" (Just "http://www.w3.org/1999/02/22-rdf-syntax-ns#") (Just "rdf")

main :: IO ()
main = do
  putStrLn "hello world"
  rdf <- TIO.readFile testText
  let xml = parseXML rdf
      elems = onlyElems xml
      rdfElems = map (findElement rdfqn) elems
      rdf = filter isJust rdfElems
  print rdf
