{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.View.Error (
    errorView
  ) where

import           P

import           Text.Blaze.Html (Html, toHtml)
import qualified Text.Blaze.Html5 as H


errorView :: Text -> Html
errorView e =
  H.main $ do
    H.h1 "Error"
    H.div $
      toHtml e
