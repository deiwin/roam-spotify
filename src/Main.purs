module Main where

import Prelude
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Roam (updateFocusedBlockString)
import Spotify (getPlaybackState, timestamp, togglePlayback, withToken, Config(..), Env)

config :: Config
config =
  Config
    { clientID: "redacted"
    , clientSecret: "redacted"
    , refreshToken: "redacted"
    }

main :: Effect Unit
main =
  runProgram do
    togglePlayback
    togglePlayback
    prefixFocusedBlockWithCurTimestamp

prefixFocusedBlockWithCurTimestamp :: ReaderT Env (ExceptT String Aff) Unit
prefixFocusedBlockWithCurTimestamp = do
  playbackState <- getPlaybackState
  updateFocusedBlockString (\s -> timestamp playbackState <> " " <> s)
    # mapExceptT liftEffect
    # flip catchError (\e -> throwError ("Failed to update focused block: " <> e))
    # lift

runProgram :: forall a. Discard a => ReaderT Env (ExceptT String Aff) a -> Effect Unit
runProgram program =
  launchAff_ do
    result <-
      runExceptT do
        (Tuple env infiniteTokenRefreshFiber) <- withToken config
        runReaderT program env
        pure infiniteTokenRefreshFiber
    case result of
      Left e -> liftEffect $ warn ("Program failed with: " <> e)
      Right fiber -> void $ joinFiber fiber
