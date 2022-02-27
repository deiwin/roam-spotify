module Spotify
  ( togglePlayback
  , module ExportAuth
  ) where

import Prelude
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.MediaType.Common (applicationJSON)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Bifunctor (lmap)
import Control.Monad.Except (ExceptT, except, throwError)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.Trans.Class (lift)
import Data.HTTP.Method (Method(PUT))
import Data.Time.Duration (Milliseconds(..))
import Spotify.Util (request, logResult)
import Spotify.Auth (getToken, Env)
import Spotify.Auth (withToken, Config(..), Env(..), TokenResponse(..)) as ExportAuth

data PlaybackState
  = PlaybackState
    { progress :: Milliseconds
    , isPlaying :: Boolean
    }

getPlaybackState :: ReaderT Env (ExceptT String Aff) PlaybackState
getPlaybackState =
  logResult' do
    token <- getToken
    response <- lift $ request (req token)
    when (response.status == StatusCode 204) (throwError "Received empty playback state")
    decodeJson response.body
      # lmap printJsonDecodeError
      # except
      # lift
  where
  logResult' = mapReaderT (logResult "get playback state")

  req token =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player"
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

togglePlayback :: ReaderT Env (ExceptT String Aff) Unit
togglePlayback = do
  (PlaybackState playbackState) <- getPlaybackState
  if playbackState.isPlaying then
    pausePlayback
  else
    resumePlayback

pausePlayback :: ReaderT Env (ExceptT String Aff) Unit
pausePlayback =
  logResult' do
    token <- getToken
    void $ lift $ request $ req token
  where
  logResult' = mapReaderT (logResult "pause playback")

  req token =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player/pause"
      , method = Left PUT
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

resumePlayback :: ReaderT Env (ExceptT String Aff) Unit
resumePlayback =
  logResult' do
    token <- getToken
    void $ lift $ request $ req token
  where
  logResult' = mapReaderT (logResult "resume playback")

  req token =
    AX.defaultRequest
      { url = "https://api.spotify.com/v1/me/player/play"
      , method = Left PUT
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization" ("Bearer " <> token)
        ]
      , responseFormat = json
      }

instance decodeJsonPlaybackState :: DecodeJson PlaybackState where
  decodeJson json = do
    obj <- decodeJson json
    progress <- Milliseconds <$> obj .: "progress_ms"
    isPlaying <- obj .: "is_playing"
    pure $ PlaybackState { progress, isPlaying }
