{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.XML.Light
import qualified Data.Map as M

testText = "../gutenberg-meta/rdf-files/cache/epub/10885/pg10885.rdf"

rdfqn = QName "RDF" (Just "http://www.w3.org/1999/02/22-rdf-syntax-ns#") (Just "rdf")

type NamespaceMap = M.Map String String

mkNamespaces :: [Attr] -> NamespaceMap
mkNamespaces attrs = M.fromList $ map mkNamespace (filter isNamespace attrs)
  where isNamespace attr = qPrefix (attrKey attr) == Just "xmlns"
        mkNamespace attr = (qName (attrKey attr), attrVal attr)

getRoot xml = do
      let parsed = parseXML xml
          elems = onlyElems parsed
      head $ mapMaybe (findElement rdfqn) elems

main :: IO ()
main = do
  putStrLn "hello world"
  rdf <- TIO.readFile testText
  let root = getRoot rdf
      attrs = elAttribs root
      ns = mkNamespaces attrs
  print ns
