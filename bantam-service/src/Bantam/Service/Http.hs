{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Http (
    ContentTypeAcceptedForm (..)
  , ContentTypeProvided (..)
  , contentTypesAcceptedForm
  , contentTypesProvidedView
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

import           Network.HTTP.Media (MediaType)

import           P


data ContentTypeAcceptedForm =
    FormUrlEncoded
  deriving (Eq, Show)

data ContentTypeProvided =
    Html
  deriving (Eq, Show)


contentTypesAcceptedForm :: NonEmpty (MediaType, ContentTypeAcceptedForm)
contentTypesAcceptedForm =
  pure ("application/x-www-form-urlencoded", FormUrlEncoded)

contentTypesProvidedView :: NonEmpty (MediaType, ContentTypeProvided)
contentTypesProvidedView =
  pure ("text/html", Html)
