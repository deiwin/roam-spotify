module KeyboardShortcuts
  ( subscribeToShortcuts
  ) where

import Prelude
import Control.Coroutine (Producer)
import Control.Coroutine as CR
import Control.Coroutine.Aff (produce', emit)
import Control.Monad.Rec.Class (forever, class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (bimap)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (length, minimum, maximum, any, all)
import Data.List (List(Nil), (:), take, zip, reverse, head, last)
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.NonEmpty ((:|))
import Data.Time.Duration (fromDuration, Seconds(..), negateDuration)
import Data.Tuple (uncurry)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (timeStamp)
import Web.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key, toEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type Shortcut
  = NonEmptyList String

shortcuts :: List Shortcut
shortcuts =
  NonEmptyList ("F13" :| "p" : Nil)
    : NonEmptyList ("F13" :| "l" : Nil)
    : Nil

subscribeToShortcuts :: forall f m. MonadRec m => Parallel f m => MonadAff m => ((List KeyboardEvent) -> m Unit) -> m Unit
subscribeToShortcuts action = CR.runProcess $ shortcutProducer `CR.connect` consumer
  where
  consumer =
    forever do
      event <- CR.await
      lift $ action event

shortcutProducer :: forall m. MonadAff m => Producer (List KeyboardEvent) m Unit
shortcutProducer = produce' \emitter -> void $ liftEffect $ HS.subscribe shortcutEmitter (emit emitter)

-- TODO how/when to unsubscribe?
-- liftEffect $ HS.unsubscribe subscription

shortcutEmitter :: HS.Emitter (List KeyboardEvent)
shortcutEmitter =
  keydownEmitter
    # (\emitter -> HS.fold (\x acc -> take maxShortcutLength (x : acc)) emitter Nil)
    # HS.filter ((_ >= minShortcutLength) <<< length)
    # HS.filter matchesShortcut
    # HS.filter fastEnough
  where
  fastEnough events =
    fromMaybe false do
      lastEvent <- head events
      firstEvent <- last events
      let
        diff =
          unInstant (timeStamp (toEvent lastEvent))
            <> negateDuration (unInstant (timeStamp (toEvent firstEvent)))
      pure (diff < fromDuration (Seconds 1.0))

  matchesShortcut events =
    shortcuts -- List (NonEmptyList String)
      <#> ( toList -- List String
            >>> reverse -- List String
            >>> zip events -- List (Tuple KeyboardEvent String)
            >>> map (bimap key identity) -- List (Tuple String String)
        )
      # any (all (uncurry (==)))

  minShortcutLength =
    shortcuts
      <#> length
      # minimum
      # fromMaybe 1

  maxShortcutLength =
    shortcuts
      <#> length
      # maximum
      # fromMaybe 1

-- TODO: Should we preventDefault when current keypresses could be a prefix for
-- one of the shortcuts? How do we retrigger when it turns out they weren't? Do
-- we have to set the listener for the capture phase for this?
keydownEmitter :: HS.Emitter KeyboardEvent
keydownEmitter =
  eventEmitter
    # map fromEvent
    # HS.filter isJust
    # map (unsafePartial fromJust)
  where
  eventEmitter =
    HS.makeEmitter \emit -> do
      target <- window >>= document <#> toEventTarget
      domEventListener <- liftEffect $ eventListener emit
      liftEffect $ addEventListener keydown domEventListener false target
      pure $ removeEventListener keydown domEventListener false target
