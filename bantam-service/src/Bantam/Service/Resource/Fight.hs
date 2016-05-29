{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Resource.Fight (
    fightsResource
  , fightResource
  , lemmasResource
  , lemmaResource
  ) where

import           Bantam.Service.Api
import           Bantam.Service.Data
import           Bantam.Service.Http
import           Bantam.Service.Path
import           Bantam.Service.View
import           Bantam.Service.View.Fight
import           Bantam.Service.Wai.Form
import           Bantam.Service.Webship

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.List (lookup)

import           P

import qualified Network.HTTP.Types as HTTP

import           Webship.Wai (ResponseBody (..))

import           X.Control.Monad.Trans.Either (hoistEither)


fightsResource :: MonadIO m => Fight m -> Email -> Resource m
fightsResource fightService _email req =
  lift $ case requestMethod req of
    "GET" -> do
      f <- currentFight fightService
      return $ case f of
        Nothing ->
          -- FIX Show _something_
          redirect loginPath ()
        Just f' ->
          redirect fightPath f'
    _ ->
      return $
        halt HTTP.status405 Empty

fightResource :: MonadIO m => Fight m -> FightId -> Email -> Resource m
fightResource fightService fightId email req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  lift $ case requestMethod req of
    "GET" -> do
      ls <- userLemmas fightService fightId email
      return
        . halt HTTP.status200
        . view accept (renderFightId fightId <> " - Fight")
        $ fightView fightId ls
    _ ->
      return $
        halt HTTP.status405 Empty

lemmasResource :: MonadIO m => Fight m -> FightId -> Email -> Resource m
lemmasResource fightService fightId email req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  case requestMethod req of
    "POST" -> do
      contentType <- hoistEither $ lookupContentType req contentTypesAcceptedForm
      m <- case contentType of
        FormUrlEncoded -> do
          f <- liftIO $ parseFormData req
          return $
            (Lemma <$> lookup "lemma" f)
      lift $ case m of
        Nothing ->
          return . halt HTTP.status400 . view accept "___" $ lemmaView fightId Nothing
        Just l -> do
          lid <- createLemma fightService fightId l
          allowUserLemma fightService fightId email lid
          return $ redirect fightPath fightId
    "GET" ->
      return
        . halt HTTP.status200
        . view accept ("Lemma - " <> renderFightId fightId <> " - Fight")
        $ lemmaView fightId Nothing
    _ ->
      return $
        halt HTTP.status405 Empty

lemmaResource :: MonadIO m => Fight m -> FightId -> LemmaId -> Email -> Resource m
lemmaResource fightService fightId lemmaId email req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  forbiddenB $ hasUserLemma fightService fightId email lemmaId
  case requestMethod req of
    "POST" -> do
      contentType <- hoistEither $ lookupContentType req contentTypesAcceptedForm
      m <- case contentType of
        FormUrlEncoded -> do
          f <- liftIO $ parseFormData req
          return $
            (Lemma <$> lookup "lemma" f)
      lift $ case m of
        Nothing ->
          return . halt HTTP.status400 . view accept "___" $ lemmaView fightId Nothing
        Just l -> do
          updateLemma fightService fightId lemmaId l
          return $ redirect fightPath fightId
    "GET" -> do
      lemma <- forbidden $ getLemma fightService fightId lemmaId
      return
        . halt HTTP.status200
        . view accept (renderLemmaId lemmaId <> " - " <> renderFightId fightId <> " - Fight")
        $ lemmaView fightId (Just (lemmaId, lemma))
    _ ->
      return $
        halt HTTP.status405 Empty
