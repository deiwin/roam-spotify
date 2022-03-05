module Main where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Roam (getFocusedBlock)
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
    focusedBlock <- lift $ mapExceptT liftEffect getFocusedBlock
    liftEffect $ logShow focusedBlock

runProgram :: forall a. Discard a => ReaderT Env (ExceptT String Aff) a -> Effect Unit
runProgram program =
  (launchAff_ <<< runExceptT) do
    (Tuple env infiniteTokenRefreshFiber) <- withToken config
    runReaderT program env
    void $ liftAff $ joinFiber infiniteTokenRefreshFiber
