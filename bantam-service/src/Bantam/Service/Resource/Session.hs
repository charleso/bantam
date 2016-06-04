{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Resource.Session (
    secure
  , secure'
  , sessionCookieKey
  ) where

import           Bantam.Service.Api.Login
import           Bantam.Service.Data
import           Bantam.Service.Path
import           Bantam.Service.Wai.Cookie
import           Bantam.Service.Webship

import           Control.Monad.Trans.Class (lift)

import           P

import           Network.Wai (Request)


secure :: Monad m => Login m -> (Email -> Resource m) -> Resource m
secure loginService resource request = do
  secure' loginService (\e' request' -> case e' of
    Nothing ->
      return $ redirect loginPath ()
    Just e ->
      resource e request') request

secure' :: Monad m => Login m -> (Maybe Email -> Resource m) -> Resource m
secure' loginService resource request = do
  let sid = getSessionId request
  s <- lift . bind (return . join) . forM sid $ getSession loginService
  resource s request

getSessionId :: Request -> Maybe SessionId
getSessionId request =
  SessionId <$> getCookie request sessionCookieKey

sessionCookieKey :: CookieKey
sessionCookieKey =
  CookieKey "JSESSIONID"
