module Main where

import Prelude
import Spotify
  ( PlaybackState(..)
  , getPlaybackState
  , togglePlayback
  , GetAccessTokenResponse(..)
  , getAccessToken
  )
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, warn)
import Effect.Aff (launchAff_)
import Data.Either (Either(..))
import Control.Monad.Except (runExceptT)

refreshToken :: String
refreshToken = "redacted"

clientID :: String
clientID = "redacted"

clientSecret :: String
clientSecret = "redacted"

main :: Effect Unit
main =
  launchAff_ do
    liftEffect $ log "🍝"
    s <-
      runExceptT do
        (GetAccessTokenResponse response) <- getAccessToken clientID clientSecret refreshToken
        let
          token = response.accessToken
        x <- getPlaybackState token
        togglePlayback token
        togglePlayback token
        pure x
    case s of
      Right (PlaybackState ps) -> do
        liftEffect $ log $ show ps.progressMs
        liftEffect $ log $ show ps.isPlaying
      Left e -> liftEffect $ warn e
