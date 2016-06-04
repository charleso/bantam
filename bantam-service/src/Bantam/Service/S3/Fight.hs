{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.S3.Fight (
    fightS3
  ) where

import           Bantam.Service.Api.Fight
import           Bantam.Service.Data

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Mismi.S3 (AWS, Address, Key (..), (//))
import qualified Mismi.S3 as S3

import           P

import           System.Random.MWC (GenIO, uniformR)

fightS3 :: GenIO -> Address -> Fight AWS
fightS3 genIO store =
  Fight
    (currentFight' store)
    (fightLemmas' store)
    (userLemmas' store)
    (getLemma' store)
    (createLemma' genIO store)
    (updateLemma' store)
    (allowUserLemma' store)
    (hasUserLemma' store)
    (inboxLemmas' store)
    (hasInboxLemma' store)
    (approveLemma' store)
    (currentLemma' store)

-----------------------

currentFight' :: Address -> AWS (Maybe FightId)
currentFight' store =
  fmap (FightId . T.strip) <$> S3.read (store /// currentKey)

fightLemmas' :: Address -> FightId -> AWS [LemmaId]
fightLemmas' store fid = do
  ls <- S3.list (store /// lemmasKey fid)
  pure . fmap LemmaId . catMaybes . fmap (S3.basename . S3.key) $ ls

userLemmas' :: Address -> FightId -> Email -> AWS [LemmaId]
userLemmas' store fid email = do
  ls <- S3.list (store /// userLemmasKey fid email)
  pure . fmap LemmaId . catMaybes . fmap (S3.basename . S3.key) $ ls

getLemma' :: Address -> FightId -> LemmaId -> AWS (Maybe Lemma)
getLemma' store fid lid =
  fmap (Lemma . T.strip) <$> S3.read (store /// lemmaKey fid lid)

createLemma' :: GenIO -> Address -> FightId -> Lemma -> AWS LemmaId
createLemma' genIO store fid l = do
  lid <- liftIO . fmap (LemmaId . T.pack . show) $
     uniformR (99999 :: Int, 99999999 :: Int) genIO
  updateLemma' store fid lid l
  pure lid

updateLemma' :: Address -> FightId -> LemmaId -> Lemma -> AWS ()
updateLemma' store fid lid l =
  void $ S3.writeWithMode S3.Overwrite (store /// lemmaKey fid lid) (renderLemma l)

allowUserLemma' :: Address -> FightId -> Email -> LemmaId -> AWS ()
allowUserLemma' store fid email lid =
  void $ S3.writeWithMode S3.Overwrite (store /// userLemmaKey fid email lid) ""

hasUserLemma' :: Address -> FightId -> Email -> LemmaId -> AWS Bool
hasUserLemma' store fid email lid =
  S3.exists (store /// userLemmaKey fid email lid)

inboxLemmas' :: Address -> FightId -> Email -> AWS [LemmaId]
inboxLemmas' store fid email = do
  ls <- S3.list (store /// inboxLemmasKey fid email)
  pure . fmap LemmaId . catMaybes . fmap (S3.basename . S3.key) $ ls

hasInboxLemma' :: Address -> FightId -> Email -> LemmaId -> AWS Bool
hasInboxLemma' store fid email lid =
  S3.exists (store /// inboxLemmaKey fid email lid)

approveLemma' :: Address -> FightId -> LemmaId -> Email -> Review -> AWS ()
approveLemma' store fid lid email review = do
  void $ S3.writeWithMode S3.Overwrite (store /// reviewLemmaKey fid lid email) (renderReview review)
  S3.delete (store /// inboxLemmaKey fid email lid)

currentLemma' :: Address -> FightId -> AWS (Maybe LemmaId)
currentLemma' store fid =
  fmap (LemmaId . T.strip) <$> S3.read (store /// currentLemmaKey fid)

-----------------------

currentKey :: Key
currentKey =
  Key "current"

fightKey :: FightId -> Key
fightKey fid =
  Key "fight" // Key (renderFightId fid)

lemmasKey :: FightId -> Key
lemmasKey fid =
  fightKey fid // Key "lemma"

lemmaKey :: FightId -> LemmaId -> Key
lemmaKey fid lid =
  lemmasKey fid // Key (renderLemmaId lid)

userLemmasKey :: FightId -> Email -> Key
userLemmasKey fid email =
  fightKey fid // Key "fighter" // Key (renderEmail email) // Key "lemma"

userLemmaKey :: FightId -> Email -> LemmaId -> Key
userLemmaKey fid email lid =
  userLemmasKey fid email // Key (renderLemmaId lid)

inboxLemmasKey :: FightId -> Email -> Key
inboxLemmasKey fid email =
  fightKey fid // Key "fighter" // Key (renderEmail email) // Key "inbox"

inboxLemmaKey :: FightId -> Email -> LemmaId -> Key
inboxLemmaKey fid email lid =
  inboxLemmasKey fid email // Key (renderLemmaId lid)

reviewLemmaKey :: FightId -> LemmaId -> Email -> Key
reviewLemmaKey fid lid email =
  fightKey fid // Key "review" // Key (renderLemmaId lid) // Key (renderEmail email)

currentLemmaKey :: FightId -> Key
currentLemmaKey fid =
  fightKey fid // Key "current-lemma"

-----------------------

(///) :: Address -> Key -> Address
(///) a k =
  S3.withKey (// k) a
infixl 3 ///
