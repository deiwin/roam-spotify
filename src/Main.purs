module Main where

import Prelude
import Control.Coroutine (Producer)
import Control.Coroutine as CR
import Control.Coroutine.Aff (produce', emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (warn, log)
import Roam (updateFocusedBlockString)
import Spotify (getPlaybackState, timestamp, togglePlayback, withToken, Config(..), Env)
import Web.Event.Event (Event, preventDefault)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key, toEvent)
import Data.Maybe (Maybe(..))
import Control.Monad.Rec.Class (forever)
import Control.Safely (replicateM_)
import Data.List (List(Nil), (:), take)

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
    -- togglePlayback
    -- togglePlayback
    -- prefixFocusedBlockWithCurTimestamp
    CR.runProcess $ keydownProducer `CR.connect` consumer
    liftEffect $ log "done"
  where
  consumer :: CR.Consumer KeyboardEvent (ReaderT Env (ExceptT String Aff)) Unit
  consumer =
    forever do
      keyboardEvent <- CR.await
      liftEffect $ preventDefault $ toEvent keyboardEvent
      -- lift $ prefixFocusedBlockWithCurTimestamp
      liftEffect $ log (key keyboardEvent)

takeFive :: CR.CoTransformer String (List String) Aff Unit
takeFive = go Nil
  where
  go acc = do
    o <- CR.cotransform acc
    lift (liftEffect (log o))
    go (take 5 (o : acc))

keydownProducer :: forall m. MonadAff m => Producer KeyboardEvent m Unit
keydownProducer =
  produce' \emitter -> do
    target <- window >>= document <#> toEventTarget
    listener <- liftEffect $ eventListener (emitKeyboardEvent emitter)
    liftEffect $ addEventListener keydown listener false target
  where
  emitKeyboardEvent emitter event = case fromEvent event of
    Nothing -> pure unit
    Just keyboardEvent -> emit emitter keyboardEvent

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
