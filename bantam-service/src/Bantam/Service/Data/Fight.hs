{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Bantam.Service.Data.Fight (
    Matches
  , Match (..)
  , Game (..)
  , Result (..)
  ---
  , emptyMatches
  , matchesReady
  , matchesCurrentLemma
  ---
  , parseMatches
  ) where

import qualified Data.Text as T

import           P

import           Bantam.Service.Data


data Matches =
  Matches {
      _matches :: [Match]
    } deriving (Eq, Show)

data Match =
  Match {
      matchPlayer1 :: Email
    , matchPlayer2 :: Email
    , matchUpcoming :: [LemmaId]
    , matchCurrent :: Maybe LemmaId
    , matchGames :: [Game]
    } deriving (Eq, Show)

data Game =
  Game {
      gameLemma :: LemmaId
    , gameResult :: Result
    } deriving (Eq, Show)

data Result =
    Winner1
  | Winner2
  | Draw
  deriving (Bounded, Enum, Eq, Show)

--------------

emptyMatches :: Matches
emptyMatches =
  Matches []

matchesReady :: Matches -> [Match]
matchesReady (Matches m) =
  flip filter m $ \m' ->
    case (matchCurrent m', matchGames m') of
      -- If there isn't a current game and no results it's still draft
      (Nothing, []) ->
        False
      _ ->
        True

matchesCurrentLemma :: Matches -> Maybe LemmaId
matchesCurrentLemma =
  head . catMaybes . fmap matchCurrent . _matches

--------------

parseMatches :: Text -> Either Text Matches
parseMatches =
  fmap Matches . traverse parseMatch . T.lines

parseMatch :: Text -> Either Text Match
parseMatch t =
  case T.splitOn "|" t of
    p1 : p2 : gs : cr : up : [] ->
      Match
        (Email p1)
        (Email p2)
        (fmap LemmaId . splitOnBlank "," $ up)
        (case cr of
          "" ->
            Nothing
          _ ->
            Just . LemmaId $ cr
            )
        <$> (for (splitOnBlank "," gs) $ \g ->
          case splitOnBlank ":" g of
            l : r : [] ->
              Game
                (LemmaId l)
                <$> (case r of
                  "D" -> pure Draw
                  "W1" -> pure Winner1
                  "W2" -> pure Winner2
                  _ -> Left $ "Invalid game result type: " <> g
                  )
            _ ->
              Left $ "Invalid game result: " <> g
          )
    _ ->
      Left $ "Invalid match: " <> t

splitOnBlank :: Text -> Text -> [Text]
splitOnBlank s t =
  case T.null t of
    True ->
      []
    False ->
      T.splitOn s t
