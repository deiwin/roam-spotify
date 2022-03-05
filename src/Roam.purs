module Roam
  ( getFocusedBlockMetadata
  , FocusedBlockMetadata(..)
  ) where

import Prelude
import Control.Monad.Except (ExceptT, except)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)

foreign import _getFocusedBlockMetadata :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe Json)

getFocusedBlockMetadata :: ExceptT String Effect (Maybe FocusedBlockMetadata)
getFocusedBlockMetadata = do
  blockJsonMaybe <- liftEffect $ _getFocusedBlockMetadata Just Nothing
  case blockJsonMaybe of
    Just blockJson ->
      blockJson
        # decodeJson
        # map Just
        # lmap printJsonDecodeError
        # except
    Nothing -> pure Nothing

data FocusedBlockMetadata
  = FocusedBlockMetadata
    { windowId :: String
    , blockUid :: String
    }

derive instance genericFocusedBlockMetadata :: Generic FocusedBlockMetadata _

instance showFocusedBlockMetadata :: Show FocusedBlockMetadata where
  show = genericShow

instance decodeJsonFocusedBlockMetadata :: DecodeJson FocusedBlockMetadata where
  decodeJson json = do
    obj <- decodeJson json
    windowId <- obj .: "window-id"
    blockUid <- obj .: "block-uid"
    pure $ FocusedBlockMetadata { windowId, blockUid }
