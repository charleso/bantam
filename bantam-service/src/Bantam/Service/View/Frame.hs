{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Bantam.Service.View.Frame (
    frame
  ) where

import           P

import           Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


frame :: Text -> Html -> Html
frame title body =
  H.html ! HA.lang "en" $ do
    H.head $ do
      H.meta ! HA.charset "utf-8"
      H.title $ toHtml title
      H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "/assets/css/bootstrap.css"
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "/assets/css/styles.css"
      H.link
        ! HA.rel "icon"
        ! HA.type_ "image/png"
        ! HA.sizes "64x64"
        ! HA.href "https://www.egison.org/images/CoqLogo.png"
    H.body $ do
      H.div ! HA.class_ "pane-header" $ do
        H.img
         ! HA.alt "CoqFight"
         ! HA.src "/assets/img/coqfight.png"
      H.div ! HA.class_ "container" $
        body
      H.div ! HA.class_ "pane-footer" $ ""
