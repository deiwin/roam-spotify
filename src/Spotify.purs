module Spotify
  ( togglePlayback
  , Config(..)
  , withToken
  , Env(..)
  , TokenResponse(..)
  ) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, warn)
import Effect.Aff (Aff, delay, forkAff, Fiber)
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
import Control.Monad.Except (ExceptT, except, mapExceptT, throwError, runExceptT)
import Control.Monad.Reader (ReaderT, ask, mapReaderT)
import Control.Monad.Trans.Class (lift)
import Data.HTTP.Method (Method(PUT, POST))
import Data.String.Base64 as B64
import Data.FormURLEncoded as FUE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..), Minutes(..), fromDuration, negateDuration)
import Effect.Ref (Ref, read, write)
import Effect.Ref (new) as Ref

data PlaybackState
  = PlaybackState
    { progress :: Milliseconds
    , isPlaying :: Boolean
    }

instance decodeJsonPlaybackState :: DecodeJson PlaybackState where
  decodeJson json = do
    obj <- decodeJson json
    progress <- Milliseconds <$> obj .: "progress_ms"
    isPlaying <- obj .: "is_playing"
    pure $ PlaybackState { progress, isPlaying }

getToken :: ReaderT Env (ExceptT String Aff) String
getToken = do
  (Env env) <- ask
  (TokenResponse tokenResponse) <- liftEffect $ read env.tokenResponseRef
  pure $ tokenResponse.accessToken

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

togglePlayback :: ReaderT Env (ExceptT String Aff) Unit
togglePlayback = do
  (PlaybackState playbackState) <- getPlaybackState
  if playbackState.isPlaying then
    pausePlayback
  else
    resumePlayback

data TokenResponse
  = TokenResponse
    { accessToken :: String
    , expiresIn :: Seconds
    }

instance decodeJsonGetAccessTokenResponse :: DecodeJson TokenResponse where
  decodeJson json = do
    obj <- decodeJson json
    accessToken <- obj .: "access_token"
    expiresIn <- Seconds <$> obj .: "expires_in"
    pure $ TokenResponse { accessToken, expiresIn }

getAccessToken :: Config -> ExceptT String Aff TokenResponse
getAccessToken (Config config) =
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
        , RequestHeader "Authorization" ("Basic " <> B64.encode (config.clientID <> ":" <> config.clientSecret))
        ]
      , responseFormat = json
      , content =
        Just
          ( FormURLEncoded
              ( FUE.fromArray
                  [ Tuple "grant_type" (Just "refresh_token")
                  , Tuple "refresh_token" (Just config.refreshToken)
                  ]
              )
          )
      }

data Config
  = Config
    { clientID :: String
    , clientSecret :: String
    , refreshToken :: String
    }

data Env
  = Env
    { tokenResponseRef :: Ref TokenResponse
    }

refreshTokenIndefinitely :: Config -> Ref TokenResponse -> ExceptT String Aff Unit
refreshTokenIndefinitely config tokenResponseRef = do
  curResponse <- liftEffect $ read tokenResponseRef
  liftEffect $ log ("Refreshing acccess token after " <> show (delayDuration curResponse))
  liftAff (delay (delayDuration curResponse))
  liftEffect $ log $ "Refreshing acccess token"
  newResp <- getAccessToken config
  liftEffect $ write newResp tokenResponseRef
  refreshTokenIndefinitely config tokenResponseRef
  where
  delayDuration :: TokenResponse -> Milliseconds
  delayDuration (TokenResponse resp) =
    resp.expiresIn
      # fromDuration
      # (_ <> negateDuration (fromDuration (Minutes 5.0)))

withToken :: Config -> ExceptT String Aff (Tuple Env (Fiber (Either String Unit)))
withToken config = do
  initialResponse <- getAccessToken config
  tokenResponseRef <- liftEffect $ Ref.new initialResponse
  infiniteFiber <-
    refreshTokenIndefinitely config tokenResponseRef
      # runExceptT
      # forkAff
      # liftAff
  pure (Tuple (Env { tokenResponseRef }) infiniteFiber)

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

logResult ::
  forall m a.
  Bind m =>
  MonadEffect m =>
  String ->
  ExceptT String m a ->
  ExceptT String m a
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
