module Spotify
  ( Token
  , PlaybackState(..)
  , getPlaybackState
  , togglePlayback
  , GetAccessTokenResponse(..)
  , getAccessToken
  ) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, warn)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Affjax as AX
import Affjax (Request, Response)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody (RequestBody(FormURLEncoded))
import Data.Either (Either(..))
import Data.MediaType.Common (applicationJSON, applicationFormURLEncoded)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Bifunctor (lmap)
import Control.Monad.Except (ExceptT, except, mapExceptT, throwError)
import Data.HTTP.Method (Method(PUT, POST))
import Data.String.Base64 as B64
import Data.FormURLEncoded as FUE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Token
  = String

data PlaybackState
  = PlaybackState
    { progressMs :: Int
    , isPlaying :: Boolean
    }

instance decodeJsonPlaybackState :: DecodeJson PlaybackState where
  decodeJson json = do
    obj <- decodeJson json
    progressMs <- obj .: "progress_ms"
    isPlaying <- obj .: "is_playing"
    pure $ PlaybackState { progressMs, isPlaying }

getPlaybackState :: Token -> ExceptT String Aff PlaybackState
getPlaybackState token =
  logResult' do
    response <- request req
    when (response.status == StatusCode 204) (throwError "Received empty playback state")
    decodeJson response.body
      # lmap printJsonDecodeError
      # except
  where
  logResult' = logResult "get playback state"

  req =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player"
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

pausePlayback :: Token -> ExceptT String Aff Unit
pausePlayback token =
  request req
    # void
    # logResult'
  where
  logResult' = logResult "pause playback"

  req =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player/pause"
      , method = Left PUT
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

resumePlayback :: Token -> ExceptT String Aff Unit
resumePlayback token =
  request req
    # void
    # logResult'
  where
  logResult' = logResult "resume playback"

  req =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player/play"
      , method = Left PUT
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

togglePlayback :: Token -> ExceptT String Aff Unit
togglePlayback token = do
  (PlaybackState playbackState) <- getPlaybackState token
  if playbackState.isPlaying then
    pausePlayback token
  else
    resumePlayback token

data GetAccessTokenResponse
  = GetAccessTokenResponse
    { accessToken :: String
    , expiresInSeconds :: Int
    }

instance decodeJsonGetAccessTokenResponse :: DecodeJson GetAccessTokenResponse where
  decodeJson json = do
    obj <- decodeJson json
    accessToken <- obj .: "access_token"
    expiresInSeconds <- obj .: "expires_in"
    pure $ GetAccessTokenResponse { accessToken, expiresInSeconds }

getAccessToken :: String -> String -> String -> ExceptT String Aff GetAccessTokenResponse
getAccessToken clientID clientSecret refreshToken =
  logResult' do
    response <- request req
    decodeJson response.body
      # lmap printJsonDecodeError
      # except
  where
  logResult' = logResult "get access token"

  req =
    AX.defaultRequest
      { url = "https://accounts.spotify.com/api/token"
      , method = Left POST
      , headers =
        [ ContentType applicationFormURLEncoded
        , RequestHeader "Authorization" ("Basic " <> B64.encode (clientID <> ":" <> clientSecret))
        ]
      , responseFormat = json
      , content =
        Just
          ( FormURLEncoded
              ( FUE.fromArray
                  [ Tuple "grant_type" (Just "refresh_token")
                  , Tuple "refresh_token" (Just refreshToken)
                  ]
              )
          )
      }

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

logResult :: forall m a. Bind m => MonadEffect m => String -> ExceptT String m a -> ExceptT String m a
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
