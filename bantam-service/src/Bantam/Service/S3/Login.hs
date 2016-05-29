{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.S3.Login (
    loginS3
  ) where

import           Bantam.Service.Api.Login
import           Bantam.Service.Data

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

import           Mismi.S3 (AWS, Address, Key (..), (//))
import qualified Mismi.S3 as S3

import           P

import           System.IO (IO)

import           Tinfoil.Data (Entropy (..))
import           Tinfoil.Random (entropy)


loginS3 :: Address -> Login AWS
loginS3 store =
  Login
    (login' store)
    (getSession' store)
    (createAccount' store)

-----------------------

login' :: Address -> Email -> Password -> AWS (Maybe SessionId)
login' store e passIn = do
  p' <- S3.read (store /// passwordKey e)
  if fmap Password p' /= Just passIn then
    pure Nothing
  else
    Just <$> newSession' store e

newSession' :: Address -> Email -> AWS SessionId
newSession' store e = do
  s <- liftIO newSession
  S3.writeOrFail (store /// sessionKey s) (renderEmail e)
  pure s
  where
    newSession :: IO SessionId
    newSession =
      SessionId . T.decodeUtf8 . B16.encode . unEntropy <$> entropy 16

getSession' :: Address -> SessionId -> AWS (Maybe Email)
getSession' store s =
  fmap Email <$> S3.read (store /// sessionKey s)

createAccount' :: Address -> Email -> Password -> AWS (Maybe SessionId)
createAccount' store e p = do
  -- This will fail if the user already exists
  r <- S3.write (store /// passwordKey e) (renderPassword p)
  case r of
    S3.WriteDestinationExists _ ->
      pure Nothing
    S3.WriteOk ->
      Just <$> newSession' store e

-----------------------

passwordKey :: Email -> Key
passwordKey e =
  Key "user" // Key (renderEmail e) // Key "password"

sessionKey :: SessionId -> Key
sessionKey s =
  Key "session" // Key (renderSessionId s)

-----------------------

(///) :: Address -> Key -> Address
(///) a k =
  S3.withKey (// k) a
infixl 3 ///
