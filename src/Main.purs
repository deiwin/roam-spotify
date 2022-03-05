module Main where

import Prelude
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow, warn)
import Roam (getFocusedBlockMetadata, FocusedBlockMetadata(..), findBlock)
import Spotify (togglePlayback, withToken, Config(..), Env)

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
    maybeFocusedBlockMetadata <- lift $ mapExceptT liftEffect getFocusedBlockMetadata
    liftEffect $ logShow maybeFocusedBlockMetadata
    case maybeFocusedBlockMetadata of
      Nothing -> liftEffect $ log "No block is focused"
      Just (FocusedBlockMetadata focusedBlockMetadata) -> do
        focusedBlock <- lift $ mapExceptT liftEffect $ findBlock focusedBlockMetadata.blockUid
        liftEffect $ logShow focusedBlock

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
