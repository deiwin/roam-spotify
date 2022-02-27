module Spotify.Auth
  ( Config(..)
  , withToken
  , Env(..)
  , TokenResponse(..)
  , getToken
  ) where

import Prelude
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, delay, forkAff, Fiber)
import Effect.Aff.Class (liftAff)
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.RequestBody (RequestBody(FormURLEncoded))
import Data.Either (Either(..))
import Data.MediaType.Common (applicationFormURLEncoded)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Bifunctor (lmap)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Data.HTTP.Method (Method(POST))
import Data.String.Base64 as B64
import Data.FormURLEncoded as FUE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds, Seconds(..), Minutes(..), fromDuration, negateDuration)
import Effect.Ref (Ref, read, write)
import Effect.Ref (new) as Ref
import Spotify.Util (request, logResult)

getToken :: ReaderT Env (ExceptT String Aff) String
getToken = do
  (Env env) <- ask
  (TokenResponse tokenResponse) <- liftEffect $ read env.tokenResponseRef
  pure $ tokenResponse.accessToken

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
