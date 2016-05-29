{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.View (
    view
  , serverError
  ) where

import           Bantam.Service.Http
import           Bantam.Service.View.Error
import           Bantam.Service.View.Frame

import           P

import           Webship.Wai (ResponseBody (..))

import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)


view :: ContentTypeProvided -> Text -> Html -> ResponseBody
view p t h =
  case p of
    Html ->
      ResponseBuilder . renderHtmlBuilder $ frame t h

serverError :: Text -> ResponseBody
serverError =
  view Html "Error" . errorView

