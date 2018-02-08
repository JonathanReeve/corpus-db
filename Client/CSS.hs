{-# LANGUAGE OverloadedStrings #-}

module Client.CSS
    ( layoutCss
    ) where

import Clay
import Prelude hiding (rem)
import Data.Text.Lazy (Text)

layoutCss :: Text
layoutCss = render $ do
  body ? minHeight (px 2000)
  element ".jumbotron" ? (textAlign $ alignSide sideCenter)
  element "#gmail" ? (marginLeft $ px 10)
  element ".nav" ? (fontSize $ rem 1.5)
