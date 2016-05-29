{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Bantam.Service
import           Bantam.Service.S3.Fight
import           Bantam.Service.S3.Login

import           BuildInfo_bantam_service

import qualified Data.Text as T

import           Mismi.Environment (discoverAWSEnv, renderRegionError)
import           Mismi.S3 (addressFromText)

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO, putStrLn)
import           System.Random.MWC (createSystemRandom)

import           X.Control.Monad.Trans.Either (newEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)


main :: IO ()
main = do
  putStrLn $ "bantam-service: " <> buildInfoVersion
  port <- lookupEnvT "PORT" (maybe (pure 3000) (first T.pack . readEither . T.unpack))
  store <- lookupEnvT "BANTAM_STORE" (maybeToRight "Invalid bantam store" . bind addressFromText)
  env <- orDie renderRegionError discoverAWSEnv
  genIO <- createSystemRandom
  Warp.runSettings
    (Warp.setPort port (Warp.setHost "127.0.0.1" Warp.defaultSettings)) $
      logStdout $ bantamService
        env
        (loginS3 store)
        (fightS3 genIO store)


lookupEnvT :: Text -> (Maybe Text -> Either Text b) -> IO b
lookupEnvT e f =
  orDie id . newEitherT . fmap (f . fmap T.pack) . lookupEnv . T.unpack $ e
