{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Resource.Fight (
    fightsResource
  , fightResource
  , lemmasResource
  , currentLemmaResource
  , lemmaResource
  , reviewResource
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

import           X.Control.Monad.Trans.Either (hoistEither, runEitherT)


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

fightResource :: (Applicative m, MonadIO m) => Fight m -> FightId -> Maybe Email -> Resource m
fightResource fightService fightId email' req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  lift $ case requestMethod req of
    "GET" -> do
      mse <- runEitherT $ getMatches fightService fightId
      case mse of
        Left (MatchesParseError e) ->
          pure . halt HTTP.status500 $ serverError e
        Right ms ->
          halt HTTP.status200 . view accept (renderFightId fightId <> " - Fight") <$>
            case email' of
              Nothing ->
                pure $ matchesView fightId ms
              Just email -> do
                (ls, inbox) <- (,)
                  <$> userLemmas fightService fightId email
                  <*> inboxLemmas fightService fightId email
                pure $ fightView fightId ms ls inbox
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

currentLemmaResource :: (Functor m, MonadIO m) => Fight m -> FightId -> Resource m
currentLemmaResource fightService fightId req = do
  lemmaId <- notFound $ currentLemma fightService fightId
  lemmaReadResource fightService fightId lemmaId req

lemmaResource :: (Functor m, MonadIO m) => Fight m -> FightId -> LemmaId -> Maybe Email -> Resource m
lemmaResource fightService fightId lemmaId email' req =
  case email' of
    Nothing -> do
      forbiddenB $ (Just lemmaId ==) <$> currentLemma fightService fightId
      lemmaReadResource fightService fightId lemmaId req
    Just email -> do
      owner <- lift $ hasUserLemma fightService fightId email lemmaId
      case owner of
        False -> do
          forbiddenB $ (Just lemmaId ==) <$> currentLemma fightService fightId
          lemmaReadResource fightService fightId lemmaId req
        True ->
          lemmaOwnerResource fightService fightId lemmaId email req

lemmaReadResource :: (Functor m, MonadIO m) => Fight m -> FightId -> LemmaId -> Resource m
lemmaReadResource fightService fightId lemmaId req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  case requestMethod req of
    "GET" -> do
      lemma <- forbidden $ getLemma fightService fightId lemmaId
      return
        . halt HTTP.status200
        . view accept (renderLemmaId lemmaId <> " - " <> renderFightId fightId <> " - Fight")
        $ lemmaReadView lemma
    _ ->
      return $
        halt HTTP.status405 Empty

lemmaOwnerResource :: (Functor m, MonadIO m) => Fight m -> FightId -> LemmaId -> Email -> Resource m
lemmaOwnerResource fightService fightId lemmaId _email req = do
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

reviewResource :: MonadIO m => Fight m -> FightId -> LemmaId -> Email -> Resource m
reviewResource fightService fightId lemmaId email req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  forbiddenB $ hasInboxLemma fightService fightId email lemmaId
  lemma <- forbidden $ getLemma fightService fightId lemmaId
  case requestMethod req of
    "POST" -> do
      contentType <- hoistEither $ lookupContentType req contentTypesAcceptedForm
      m <- case contentType of
        FormUrlEncoded -> do
          f <- liftIO $ parseFormData req
          return $
            (Review <$> lookup "review" f)
      lift $ case m of
        Nothing ->
          return . halt HTTP.status400 $ showReview accept lemma
        Just review -> do
          approveLemma fightService fightId lemmaId email review
          return $ redirect fightPath fightId
    "GET" -> do
      return . halt HTTP.status200 $ showReview accept lemma
    _ ->
      return $
        halt HTTP.status405 Empty
  where
    showReview accept lemma =
      view accept (renderLemmaId lemmaId <> " - Review - " <> renderFightId fightId <> " - Fight")
        $ reviewView fightId lemmaId lemma
