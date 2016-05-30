{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Bantam.Service.Data (
    Email (..)
  , Password (..)
  , SessionId (..)
  , FightId (..)
  , _FightId
  , LemmaId (..)
  , _LemmaId
  , Lemma (..)
  , Review (..)
  ) where

import           P

import           Portmanteau.Lens


newtype Email =
  Email {
      renderEmail :: Text
    } deriving (Eq, Show)

newtype Password =
  Password {
      renderPassword :: Text
    } deriving (Eq, Show)

newtype SessionId =
  SessionId {
      renderSessionId :: Text
    } deriving (Eq, Show)

newtype FightId =
  FightId {
      renderFightId :: Text
    } deriving (Eq, Show)

makeIso ''FightId

newtype LemmaId =
  LemmaId {
      renderLemmaId :: Text
    } deriving (Eq, Show)

makeIso ''LemmaId

data Lemma =
  Lemma {
      renderLemma :: Text
    } deriving (Eq, Show)

data Review =
  Review {
      renderReview :: Text
    } deriving (Eq, Show)
