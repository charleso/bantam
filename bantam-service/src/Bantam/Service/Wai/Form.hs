{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Wai.Form (
    parseFormData
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text.Encoding as T

import           Network.Wai (Request)
import           Network.Wai.Parse (lbsBackEnd, parseRequestBody)

import           P


parseFormData :: MonadIO m => Request -> m [(Text, Text)]
parseFormData =
  liftIO
    . fmap (fmap (\(k, v) -> (T.decodeUtf8 k, T.decodeUtf8 v)) . fst)
    . parseRequestBody lbsBackEnd
