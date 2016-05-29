{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Resource.Login (
    loginResource
  ) where

import           Bantam.Service.Api.Login
import           Bantam.Service.Data
import           Bantam.Service.Path
import           Bantam.Service.Http
import           Bantam.Service.View
import           Bantam.Service.View.Login
import           Bantam.Service.Wai.Cookie
import           Bantam.Service.Wai.Form
import           Bantam.Service.Webship
import           Bantam.Service.Resource.Session

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.List (lookup)

import           P

import           Network.Wai (mapResponseHeaders)
import qualified Network.HTTP.Types as HTTP

import           Webship.Wai (ResponseBody (..))

import           X.Control.Monad.Trans.Either (hoistEither)


loginResource :: MonadIO m => Login m -> Resource m
loginResource loginService req = do
  accept <- hoistEither $ lookupAccept req contentTypesProvidedView
  case requestMethod req of
    "POST" -> do
      contentType <- hoistEither $ lookupContentType req contentTypesAcceptedForm
      m <- case contentType of
        FormUrlEncoded -> do
          f <- liftIO $ parseFormData req
          return $ (,)
            <$> (Email <$> lookup "email" f)
            <*> (Password <$> lookup "password" f)
      lift $ case m of
        Nothing ->
          return . halt HTTP.status400 . view accept "Login" $ loginView Nothing
        Just (e, p) -> do
          s <- login loginService e p
          return $ case s of
            Nothing ->
              halt HTTP.status200 . view accept "Login" $ loginView (Just e)
            Just s' ->
              mapResponseHeaders (setCookieHeader (makeSimpleCookie sessionCookieKey (renderSessionId s')) :) $
                redirect fightsPath ()
    "GET" ->
      return $
        halt HTTP.status200 . view accept "Login" $ loginView Nothing
    _ ->
      return $
        halt HTTP.status405 Empty
