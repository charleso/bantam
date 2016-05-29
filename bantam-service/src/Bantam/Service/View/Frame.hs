{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.View.Frame (
    frame
  ) where

import           P

import           Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


frame :: Text -> Html -> Html
frame title body =
  H.html $ do
    H.head $ do
      H.title $ toHtml title
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
      H.link
        ! HA.rel "icon"
        ! HA.type_ "image/png"
        ! HA.sizes "64x64"
        ! HA.href "https://www.egison.org/images/CoqLogo.png"
    H.body $ do
      H.div ! HA.class_ "pane-header" $ do
        H.div ! HA.class_ "container" $
          H.img
           ! HA.alt "Coq"
           ! HA.src "https://www.egison.org/images/CoqLogo.png"
      H.div ! HA.class_ "container" $
        body
