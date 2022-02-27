module Main where

import Prelude
import Spotify
  ( togglePlayback
  , withToken
  , Config(..)
  , Env
  )
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Aff.Class (liftAff)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
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
  runProgram do
    togglePlayback
    togglePlayback

runProgram :: forall a. Discard a => ReaderT Env (ExceptT String Aff) a -> Effect Unit
runProgram program =
  (launchAff_ <<< runExceptT) do
    (Tuple env infiniteTokenRefreshFiber) <- withToken config
    runReaderT program env
    void $ liftAff $ joinFiber infiniteTokenRefreshFiber
