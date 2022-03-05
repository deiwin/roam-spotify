module Roam
  ( getFocusedBlock
  , Block(..)
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

foreign import _getFocusedBlock :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe Json)

getFocusedBlock :: ExceptT String Effect (Maybe Block)
getFocusedBlock = do
  blockJsonMaybe <- liftEffect $ _getFocusedBlock Just Nothing
  case blockJsonMaybe of
    Just blockJson ->
      blockJson
        # decodeJson
        # map Just
        # lmap printJsonDecodeError
        # except
    Nothing -> pure Nothing

data Block
  = Block
    { windowId :: String
    , blockUid :: String
    }

derive instance genericBlock :: Generic Block _

instance showBlock :: Show Block where
  show = genericShow

instance decodeJsonBlock :: DecodeJson Block where
  decodeJson json = do
    obj <- decodeJson json
    windowId <- obj .: "window-id"
    blockUid <- obj .: "block-uid"
    pure $ Block { windowId, blockUid }
