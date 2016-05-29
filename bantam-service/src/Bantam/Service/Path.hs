{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Path (
    encodedPath
  , encodedPathText
  , loginPath
  , fightsPath
  , fightPath
  , lemmasPath
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

fightsPath :: Path ()
fightsPath =
  seg "fight"

fightPath :: Path FightId
fightPath =
  seg "fight" *| (_FightId |$| var)

lemmasPath :: Path FightId
lemmasPath =
  fightPath |* seg "lemma"

lemmaPath :: Path (FightId, LemmaId)
lemmaPath =
  lemmasPath |*| (_LemmaId |$| var)

reviewPath :: Path (FightId, LemmaId)
reviewPath =
  fightPath |* seg "review" |*| (_LemmaId |$| var)
