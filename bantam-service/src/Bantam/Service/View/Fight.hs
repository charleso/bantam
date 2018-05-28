{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.View.Fight (
    fightView
  , matchesView
  , lemmaView
  , lemmaReadView
  , reviewView
  ) where

import           Bantam.Service.Data
import           Bantam.Service.Data.Fight
import           Bantam.Service.Path

import           P

import           Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


fightView :: FightId -> Matches -> [LemmaId] -> [LemmaId] -> Html
fightView fid m lemmas inbox =
  H.main $ do
    H.a ! HA.class_ "btn btn-link pull-right"
      ! HA.href (H.textValue $ encodedPathText lemmasPath fid) $
      "Create"
    matchesTable fid m
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

matchesView :: FightId -> Matches -> Html
matchesView fid m =
  H.main $
    matchesTable fid m

matchesTable :: FightId -> Matches -> Html
matchesTable fid ms =
  case matchesReady ms of
    [] ->
      mempty
    ms' -> do
      H.h1 "Matches"
      H.table
        ! HA.style "width: 100%;"
        $ do
        H.tr $ do
          H.th "Player 1"
          H.th "Player 2"
          H.th "Draws"
          H.th "Remaining"
          H.th "Current"
        for_ ms' $ \m ->
          H.tr $ do
            let
              rl l =
                H.a
                  ! HA.href (H.textValue $ encodedPathText lemmaPath (fid, l)) $
                  (toHtml . mconcat $ [renderLemmaId $ l, " "])
              rp r h =
                mconcat [
                    toHtml $ renderEmail h
                  , " ("
                  , mconcat . with (matchGames m) $
                      \(Game l r') -> if r == r' then rl l else ""
                  , ")"
                  ]
            H.td $ rp Winner1 (matchPlayer1 m)
            H.td $ rp Winner2 (matchPlayer2 m)
            H.td . mconcat . with (matchGames m) $ \(Game l r) ->
              case r of
                Draw ->
                  rl l
                _ ->
                  ""
            -- Don't link to the lemmas, they're not public yet
            H.td .  toHtml . renderIntegral . length $ matchUpcoming m
            H.td $ case matchCurrent m of
              Nothing ->
                ""
              Just c ->
                rl c

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
          ! HA.class_ "form-control lemma"
          ! HA.name "lemma"
          $
          maybe (pure ()) (toHtml . renderLemma . snd) l

      H.div ! HA.class_ "form-group" $
        H.button ! HA.class_ "btn btn-primary" ! HA.type_ "submit" $
          "Save"

lemmaReadView :: Lemma -> Html
lemmaReadView l =
  H.main $ do
    H.h1 "Lemma"

    H.label ! HA.for "lemma" $
      "Lemma"
    H.textarea
      ! HA.class_ "form-control lemma"
      ! HA.name "lemma"
      ! HA.readonly "true"
      $
      toHtml . renderLemma $ l

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
          ! HA.class_ "form-control lemma"
          ! HA.name "lemma"
          ! HA.readonly "true"
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
