{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Wai.Cookie (
    CookieKey (..)
  , makeSimpleCookie
  , setCookieHeader
  , deleteCookieHeader
  , getCookie
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSL
import           Data.List (lookup)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Network.Wai (Request (..))
import           Network.HTTP.Types (Header)

import           P

import           Web.Cookie (SetCookie)
import qualified Web.Cookie as WC


newtype CookieKey =
  CookieKey {
      renderCookieKey :: Text
    } deriving (Eq, Show)


-- SetCookie has both a hidden constructor _and_ a Data.Default instance that's useless
makeSimpleCookie :: CookieKey -> Text -> SetCookie
makeSimpleCookie k v =
  WC.def {
      WC.setCookieName = T.encodeUtf8 $ renderCookieKey k
    , WC.setCookieValue = T.encodeUtf8 v
    }


setCookieHeader :: SetCookie -> Header
setCookieHeader c =
  ("Set-Cookie", BSL.toStrict . BSL.toLazyByteString $ WC.renderSetCookie c)

deleteCookieHeader :: CookieKey -> Header
deleteCookieHeader n =
  setCookieHeader $ (makeSimpleCookie n "") {
      WC.setCookieExpires = Just $ posixSecondsToUTCTime 0
    }

getCookie :: Request -> CookieKey -> Maybe Text
getCookie r c =
  lookup "Cookie" (requestHeaders r)
    >>= fmap T.decodeUtf8 . lookup (T.encodeUtf8 $ renderCookieKey c) . WC.parseCookies

