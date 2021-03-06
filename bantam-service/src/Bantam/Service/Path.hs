{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Path (
    encodedPath
  , encodedPathText
  , loginPath
  , registrationPath
  , imgPath
  , cssPath
  , fightsPath
  , fightPath
  , lemmasPath
  , currentLemmaPath
  , lemmaPath
  , reviewPath
  ) where

import           Bantam.Service.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

import qualified Network.HTTP.Types as HTTP

import           P

import           Portmanteau.Core
import           Portmanteau.Lens ((|$|))

import           Webship.Path (Path, Path', encodePath, seg, var)


encodedPath :: Path' e a -> a -> ByteString
encodedPath p a =
  BSL.toStrict . BS.toLazyByteString . HTTP.encodePathSegments $ encodePath p a

encodedPathText :: Path' e a -> a -> Text
encodedPathText p =
  T.decodeUtf8 . encodedPath p

----------------------------

loginPath :: Path ()
loginPath =
  seg "login"

registrationPath :: Path ()
registrationPath =
  seg "registration"

fightsPath :: Path ()
fightsPath =
  seg "fight"

fightPath :: Path FightId
fightPath =
  seg "fight" *| (_FightId |$| var)

lemmasPath :: Path FightId
lemmasPath =
  fightPath |* seg "lemma"

currentLemmaPath :: Path FightId
currentLemmaPath =
  fightPath |* seg "current"

lemmaPath :: Path (FightId, LemmaId)
lemmaPath =
  lemmasPath |*| (_LemmaId |$| var)

reviewPath :: Path (FightId, LemmaId)
reviewPath =
  fightPath |* seg "review" |*| (_LemmaId |$| var)

-- FIX Just use a raw Middleware

imgPath :: Path Text
imgPath =
  seg "assets" *| seg "img" *| var

cssPath :: Path Text
cssPath =
  seg "assets" *| seg "css" *| var
