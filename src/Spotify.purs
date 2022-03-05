module Spotify
  ( togglePlayback
  , getPlaybackState
  , PlaybackState(..)
  , timestamp
  , module ExportAuth
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (ExceptT, except, throwError)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Array (replicate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(PUT))
import Data.Int (round)
import Data.MediaType.Common (applicationJSON)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Hours(..), Milliseconds(..), Minutes(..), Seconds(..), toDuration)
import Effect.Aff (Aff)
import Spotify.Auth (getToken, Env)
import Spotify.Auth (withToken, Config(..), Env(..), TokenResponse(..)) as ExportAuth
import Spotify.Util (request, logResult)

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

timestamp :: PlaybackState -> String
timestamp (PlaybackState playbackState) =
  (format identity hours)
    <> ":"
    <> (format (_ `mod` 60) minutes)
    <> ":"
    <> (format (_ `mod` 60) seconds)
  where
  (Hours hours) = toDuration playbackState.progress

  (Minutes minutes) = toDuration playbackState.progress

  (Seconds seconds) = toDuration playbackState.progress

  format :: (Int -> Int) -> Number -> String
  format f = round >>> f >>> show >>> leftPad 2 '0'

  leftPad :: Int -> Char -> String -> String
  leftPad minCount char string
    | length string >= minCount = string
    | otherwise =
      minCount - (length string)
        # flip replicate char
        # fromCharArray
        # (_ <> string)

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
