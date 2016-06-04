{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bantam.Service.Webship (
    module X
  , ResourceDenied (..)
  , Resource
  , inconceivable
  , serverError
  , lookupAccept
  , lookupContentType
  , redirect
  , forbidden
  , forbiddenB
  , notFound
  , halt
  , resourceToWaiT
  ) where

import           Bantam.Service.Path
import           Bantam.Service.View

import           Control.Monad.Trans.Class (lift)

import           Data.ByteString (ByteString)
import           Data.List (lookup)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           Network.Wai (Application, Request (..), Response, responseLBS)
import           Network.Wai as X (requestMethod)
import           Network.HTTP.Media
import qualified Network.HTTP.Types as HTTP

import           P
import qualified Prelude as Unsafe (error)

import           System.IO (IO)

import           Webship.Path (Path)
import           Webship.Wai (ResponseBody (..), toWaiResponse)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)


data ResourceDenied =
    UnsupportedAcceptType ByteString
  | UnsupportedContentType ByteString
  | Forbidden
  | NotFound

type Resource m = Request -> EitherT ResourceDenied m Response


inconceivable :: a
inconceivable =
  Unsafe.error "The impossible happened"

lookupAccept :: Request -> NonEmpty (MediaType, a) -> Either ResourceDenied a
lookupAccept req known =
  case lookup HTTP.hAccept $ requestHeaders req of
    Nothing ->
      Right . snd $ NE.head known
    Just t ->
      maybeToRight (UnsupportedAcceptType t) . mapAcceptMedia (NE.toList known) $ t

lookupContentType :: Request -> NonEmpty (MediaType, a) -> Either ResourceDenied a
lookupContentType req known =
  case lookup HTTP.hContentType $ requestHeaders req of
    Nothing ->
      Right . snd $ NE.head known
    Just t ->
      maybeToRight (UnsupportedContentType t) . mapContentMedia (NE.toList known) $ t

forbiddenB :: Monad m => m Bool -> EitherT ResourceDenied m ()
forbiddenB =
  forbidden . bind (return . bool Nothing (Just ()))

forbidden :: Monad m => m (Maybe a) -> EitherT ResourceDenied m a
forbidden =
  bind (hoistEither . maybeToRight Forbidden) . lift

notFound :: Monad m => m (Maybe a) -> EitherT ResourceDenied m a
notFound =
  bind (hoistEither . maybeToRight NotFound) . lift

redirect :: Path a -> a -> Response
redirect p a =
  responseLBS HTTP.status302 [(HTTP.hLocation, encodedPath p a)] ""

halt :: HTTP.Status -> ResponseBody -> Response
halt s b =
  toWaiResponse s [] b

resourceToWaiT :: Functor m => (Request -> m Response -> IO Response) -> Resource m -> Application
resourceToWaiT run fs req respond = do
  bind respond . run req $ do
    flip fmap (runEitherT $ fs req) $ \x -> case x of
      Left e -> case e of
        UnsupportedAcceptType _ ->
          halt HTTP.status406 $ serverError "Not acceptable"
        UnsupportedContentType _ ->
          halt HTTP.status415 $ serverError "Unsupport content-type"
        Forbidden ->
          halt HTTP.status403 $ serverError "Permission denied"
        NotFound ->
          halt HTTP.status404 $ serverError "Page not found"
      Right r ->
        r
