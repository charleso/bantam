{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.View.Fight (
    fightView
  , lemmaView
  , reviewView
  ) where

import           Bantam.Service.Data
import           Bantam.Service.Path

import           P

import           Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


fightView :: FightId -> [LemmaId] -> [LemmaId] -> Html
fightView fid lemmas inbox =
  H.main $ do
    H.a ! HA.class_ "btn btn-link pull-right"
      ! HA.href (H.textValue $ encodedPathText lemmasPath fid) $
      "Create"
    H.h1 "Lemmas"
    for_ lemmas $ \lid ->
      H.div ! HA.class_ "form-group" $
        H.a ! HA.class_ "btn btn-primary"
          ! HA.href (H.textValue $ encodedPathText lemmaPath (fid, lid)) $
          toHtml (renderLemmaId lid)
    when (not . null $ inbox) $ do
      H.h2 "Inbox"
      for_ inbox $ \lid ->
        H.div ! HA.class_ "form-group" $
          H.a ! HA.class_ "btn btn-primary"
            ! HA.href (H.textValue $ encodedPathText reviewPath (fid, lid)) $
            toHtml (renderLemmaId lid)

lemmaView :: FightId -> Maybe (LemmaId, Lemma) -> Html
lemmaView fid l =
  H.main $ do
    H.h1 "Lemma"
    H.form
      ! HA.action (H.textValue $ maybe (encodedPathText lemmasPath fid) (encodedPathText lemmaPath . (,) fid . fst) l)
      ! HA.method "post"
      $ do

      H.div ! HA.class_ "form-group" $ do
        H.label ! HA.for "lemma" $
          "Lemma"
        H.textarea
          ! HA.class_ "form-control"
          ! HA.name "lemma"
          $
          maybe (pure ()) (toHtml . renderLemma . snd) l

      H.div ! HA.class_ "form-group" $
        H.button ! HA.class_ "btn btn-primary" ! HA.type_ "submit" $
          "Save"

reviewView :: FightId -> LemmaId -> Lemma -> Html
reviewView fid lid lemma =
  H.main $ do
    H.h1 "Lemma"
    H.form
      ! HA.action (H.textValue $ encodedPathText reviewPath (fid, lid))
      ! HA.method "post"
      $ do

      H.div ! HA.class_ "form-group" $ do
        H.label ! HA.for "lemma" $
          "Lemma"
        H.textarea
          ! HA.class_ "form-control"
          ! HA.name "lemma"
          ! HA.disabled "true"
          $
          toHtml . renderLemma $ lemma

      H.div ! HA.class_ "form-group" $ do
        H.label ! HA.for "lemma" $
          "Comment"
        H.input
          ! HA.class_ "form-control"
          ! HA.name "review"

      H.div ! HA.class_ "form-group" $
        H.button ! HA.class_ "btn btn-primary" ! HA.type_ "submit" $
          "Review"
