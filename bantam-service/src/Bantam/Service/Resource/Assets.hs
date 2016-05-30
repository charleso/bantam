{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Bantam.Service.Resource.Assets (
    assetsResource
  ) where

import           Bantam.Service.Webship

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed (embedDir)
import           Data.List (lookup)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Request (..), responseLBS)

import           Tinfoil.Hash (hashSHA256)
import           Tinfoil.Data.Hash (Hash (..))

import           X.Control.Monad.Trans.Either (left)


assetsResource :: Monad m => Text -> Resource m
assetsResource _ req = do
  case flip M.lookup assets . pathInfo $ req of
    Nothing ->
      left NotFound
    Just (etag, b) ->
      return $ case lookup "If-None-Match" . requestHeaders $ req of
        Just _ ->
          responseLBS HTTP.status304 [] ""
        Nothing ->
          responseLBS HTTP.status200 [("ETag", etag)] $ b

assets :: M.Map [Text] (ByteString, BSL.ByteString)
assets =
  M.fromList
    . fmap (bimap (("assets" :) . HTTP.decodePathSegments . T.encodeUtf8 . T.pack) calculateETag)
    $ $(embedDir "assets")
  where
    calculateETag b =
      (B16.encode . unHash . hashSHA256 $ b, BSL.fromStrict b)
