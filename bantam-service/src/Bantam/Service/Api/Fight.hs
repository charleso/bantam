{-# LANGUAGE NoImplicitPrelude #-}
module Bantam.Service.Api.Fight (
    Fight (..)
  , MatchesError (..)
  , currentLemma
  ) where

import           Bantam.Service.Data
import           Bantam.Service.Data.Fight

import           P

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)


data MatchesError =
    MatchesParseError Text
  deriving (Eq, Show)

data Fight m =
  Fight {
      currentFight :: m (Maybe FightId)
    , fightLemmas :: FightId -> m [LemmaId]
    , userLemmas :: FightId -> Email -> m [LemmaId]
    , getLemma :: FightId -> LemmaId -> m (Maybe Lemma)
    , createLemma :: FightId -> Lemma -> m LemmaId
    , updateLemma :: FightId -> LemmaId -> Lemma -> m ()
    , allowUserLemma :: FightId -> Email -> LemmaId -> m ()
    , hasUserLemma :: FightId -> Email -> LemmaId -> m Bool
    , inboxLemmas :: FightId -> Email -> m [LemmaId]
    , hasInboxLemma :: FightId -> Email -> LemmaId -> m Bool
    , approveLemma :: FightId -> LemmaId -> Email -> Review -> m ()
    , getMatches :: FightId -> EitherT MatchesError m Matches
    }

currentLemma :: Functor m => Fight m -> FightId -> m (Maybe LemmaId)
currentLemma f =
  -- FIX We shouldn't throw away the error here, but this is to make the API backwards compatible for now
  fmap (either (\_ -> Nothing) matchesCurrentLemma) . runEitherT . getMatches f
