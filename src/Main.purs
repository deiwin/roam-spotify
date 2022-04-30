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
import Effect.Console (warn, log, errorShow)
import KeyboardShortcuts (subscribeToShortcuts)
import Roam (updateFocusedBlockString)
import Spotify (Config(..), Env, getPlaybackState, timestamp, withToken, togglePlayback)
import Web.UIEvent.KeyboardEvent (key)

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
    subscribeToShortcuts action
    liftEffect $ log "done"
  where
  action event = do
    logErrAndContinue do
      togglePlayback
      togglePlayback
      prefixFocusedBlockWithCurTimestamp
    liftEffect $ log (show (key <$> event))

  logErrAndContinue m =
    catchError m \e -> do
      liftEffect $ errorShow e
      pure unit

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
