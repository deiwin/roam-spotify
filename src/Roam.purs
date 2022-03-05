module Roam
  ( updateFocusedBlockString
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
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
import Effect.Console (log)

type Uid
  = String

data Block
  = Block
    { id :: Int
    , string :: String
    }

data FocusedBlockMetadata
  = FocusedBlockMetadata
    { windowId :: String
    , blockUid :: Uid
    }

foreign import _getFocusedBlockMetadata :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe Json)

foreign import _findBlock :: Uid -> Effect Json

foreign import setBlockString :: String -> Uid -> Effect Unit

updateFocusedBlockString :: (String -> String) -> ExceptT String Effect Unit
updateFocusedBlockString f = do
  (FocusedBlockMetadata focusedBlockMetadata) <-
    getFocusedBlockMetadata
      >>= note "No block is currently focused"
  liftEffect $ log ("Will update focused block with uid " <> focusedBlockMetadata.blockUid)
  updateBlockString f (focusedBlockMetadata.blockUid)

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

findBlock :: Uid -> ExceptT String Effect (Maybe Block)
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

updateBlockString :: (String -> String) -> Uid -> ExceptT String Effect Unit
updateBlockString f uid = do
  (Block block) <-
    findBlock uid
      >>= note ("Did not find a block with uid: " <> uid)
  let
    logMetadata =
      { uid: uid
      , currentMessage: block.string
      , newMessage: f block.string
      }
  liftEffect $ log ("Found block to update " <> show (logMetadata))
  liftEffect $ setBlockString (f block.string) uid
  liftEffect $ log "Block updated"

note :: forall e m a. MonadThrow e m => e -> Maybe a -> m a
note e = case _ of
  Nothing -> throwError e
  Just x -> pure x

derive instance genericBlock :: Generic Block _

instance showBlock :: Show Block where
  show = genericShow

instance decodeJsonBlock :: DecodeJson Block where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    string <- obj .: "string"
    pure $ Block { id, string }

derive instance genericFocusedBlockMetadata :: Generic FocusedBlockMetadata _

instance showFocusedBlockMetadata :: Show FocusedBlockMetadata where
  show = genericShow

instance decodeJsonFocusedBlockMetadata :: DecodeJson FocusedBlockMetadata where
  decodeJson json = do
    obj <- decodeJson json
    windowId <- obj .: "window-id"
    blockUid <- obj .: "block-uid"
    pure $ FocusedBlockMetadata { windowId, blockUid }
