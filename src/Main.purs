module Main where

import Prelude
import Spotify
  ( togglePlayback
  , withToken
  , Config(..)
  )
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (launchAff_, joinFiber)
import Effect.Aff.Class (liftAff)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Tuple (Tuple(..))

config :: Config
config =
  Config
    { clientID: "redacted"
    , clientSecret: "redacted"
    , refreshToken: "redacted"
    }

main :: Effect Unit
main =
  launchAff_ do
    liftEffect $ log "🍝"
    runExceptT do
      (Tuple env infiniteTokenRefreshFiber) <- withToken config
      runReaderT program env
      void $ liftAff $ joinFiber infiniteTokenRefreshFiber
  where
  program = do
    togglePlayback
    togglePlayback
