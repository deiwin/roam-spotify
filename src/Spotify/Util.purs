module Spotify.Util
  ( request
  , logResult
  ) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, warn)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Affjax as AX
import Affjax (Request, Response)
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Control.Monad.Except (ExceptT, except, mapExceptT, throwError)

request :: forall a. Request a -> ExceptT String Aff (Response a)
request req = do
  result <- liftAff $ AX.request (req)
  response <-
    result
      # lmap AX.printError
      # except
  case response.status of
    StatusCode x
      | x >= 200 && x < 300 -> pure response
      | otherwise -> throwError "Expected a 2xx response code"

logResult :: forall m a. MonadEffect m => String -> ExceptT String m a -> ExceptT String m a
logResult description =
  mapExceptT
    ( _
        >>= ( case _ of
              x@(Left e) -> do
                liftEffect $ warn ("Failed to " <> description <> ": " <> e)
                pure x
              x@(Right _) -> do
                liftEffect $ log ("Succeeded to " <> description)
                pure x
          )
    )
