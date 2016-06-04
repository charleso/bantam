{-# LANGUAGE NoImplicitPrelude #-}
module Bantam.Service.Api.Fight (
    Fight (..)
  ) where

import           Bantam.Service.Data

import           P


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
    , currentLemma :: FightId -> m (Maybe LemmaId)
    }
