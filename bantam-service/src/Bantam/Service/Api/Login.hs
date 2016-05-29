{-# LANGUAGE NoImplicitPrelude #-}
module Bantam.Service.Api.Login (
    Login (..)
  ) where

import           Bantam.Service.Data

import           P


data Login m =
  Login {
      login :: Email -> Password -> m (Maybe SessionId)
    , getSession :: SessionId -> m (Maybe Email)
    }
