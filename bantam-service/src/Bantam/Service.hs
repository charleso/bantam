{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service (
    bantamService
  ) where

import           Bantam.Service.Api
import           Bantam.Service.Path
import           Bantam.Service.Resource.Fight
import           Bantam.Service.Resource.Login
import           Bantam.Service.Resource.Session
import           Bantam.Service.Webship

import           Control.Monad.IO.Class (MonadIO)

import           Mismi (Env)
import           Mismi.S3 (AWS, renderError, runAWS)

import           P

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Application)

import           Webship.Path (root)
import           Webship.Route (RoutingSpec, (@>), (#>))
import           Webship.Route.Wai (routesToWai)

import           X.Control.Monad.Trans.Either (left, runEitherT)


bantamService :: Env -> Login AWS -> Fight AWS -> Application
bantamService env loginService fightService =
  routesToWai
    (resourceToWaiT (\_ ->
      fmap (either (halt HTTP.status500 . serverError . renderError) id) . runEitherT . runAWS env)
      )
    (bantamRoutes loginService fightService)
    (\_ -> left NotFound)

bantamRoutes :: MonadIO m => Login m -> Fight m -> RoutingSpec (Resource m) ()
bantamRoutes loginService fightService = do
  loginPath @> loginResource loginService
  root @> secure loginService (fightsResource fightService)
  fightsPath @> secure loginService (fightsResource fightService)
  -- FIX Better combinators would help here
  fightPath #> (\fid -> secure loginService (fightResource fightService fid))
  lemmasPath #> (\fid -> secure loginService (lemmasResource fightService fid))
  lemmaPath #> (\(fid, lid) -> secure loginService (lemmaResource fightService fid lid))
