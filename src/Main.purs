module Main where

import Prelude
import Spotify (PlaybackState(..), getPlaybackState)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, warn)
import Effect.Aff (launchAff_)
import Data.Either (Either(..))
import Control.Monad.Except (runExceptT)

main :: Effect Unit
main =
  launchAff_ do
    liftEffect $ log "🍝"
    s <-
      getPlaybackState "<redacted>"
        # runExceptT
    case s of
      Right (PlaybackState ps) -> do
        liftEffect $ log $ show ps.progressMs
        liftEffect $ log $ show ps.isPlaying
      Left e -> liftEffect $ warn e
