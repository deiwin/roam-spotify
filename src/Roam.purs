module Roam
  ( getFocusedBlockMetadata
  , FocusedBlockMetadata(..)
  , findBlock
  , Block(..)
  ) where

import Prelude
import Control.Monad.Except (ExceptT, except, throwError)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)

foreign import _getFocusedBlockMetadata :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe Json)

foreign import _findBlock :: String -> Effect Json

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

findBlock :: String -> ExceptT String Effect (Maybe Block)
findBlock uid = do
  json <- liftEffect $ _findBlock uid
  json
    # decodeJson
    # lmap printJsonDecodeError
    >>= extractBlock
    # except
  where
  extractBlock :: Array (Array Block) -> Either String (Maybe Block)
  extractBlock result =
    runMaybeT do
      blockResult <- case result of
        [] -> empty
        [ x ] -> pure x
        _ -> throwError "Expecting a single block as a result"
      case blockResult of
        [] -> throwError "Expecting block information in nested array"
        [ x ] -> pure x
        _ -> throwError "Expecting a single element in nested array"

data Block
  = Block
    { id :: Int
    }

derive instance genericBlock :: Generic Block _

instance showBlock :: Show Block where
  show = genericShow

instance decodeJsonBlock :: DecodeJson Block where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    pure $ Block { id }

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
