module Utils where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Either (Either(..))
import Data.Array (zip, (..), length)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Web.HTML as Html
import Web.HTML.Window as Window
import React.Basic.Events (handler, handler_, EventHandler)
import React.Basic.DOM.Events (capture, capture_, nativeEvent)

import Data.MediaType.Common as MediaType
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DataTransfer as DataTransfer

alert :: forall m. MonadEffect m => String -> m Unit
alert msg = liftEffect (Window.alert msg =<< Html.window)

log :: forall m. MonadEffect m => String -> m Unit
log msg = liftEffect $ Console.log msg

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = zip (0 .. (length xs - 1)) xs

rejectDrop :: EventHandler
rejectDrop = handler_ $ pure unit

acceptDrop :: EventHandler
acceptDrop = capture_ $ pure unit

dropHandler :: forall a. DecodeJson a => (a -> Effect Unit) -> EventHandler
dropHandler handle = capture nativeEvent \evt ->
  case DragEvent.fromEvent evt of
    Nothing -> pure unit
    Just dragEvt -> do
      doc <- DataTransfer.getData
        MediaType.applicationJSON
        (DragEvent.dataTransfer dragEvt)

      case jsonParser doc of
        Left err -> log err
        Right json -> case decodeJson json of
          Left err -> log $ show err
          Right dragData -> handle dragData

dragHandler :: forall a. EncodeJson a => Effect a -> EventHandler
dragHandler mkDragData = handler nativeEvent \evt ->
  case DragEvent.fromEvent evt of
    Nothing -> pure unit
    Just dragEvt -> do
      dragData <- mkDragData

      DataTransfer.setData
        MediaType.applicationJSON
        (stringify $ encodeJson dragData)
        (DragEvent.dataTransfer dragEvt)

      DataTransfer.setDropEffect
        DataTransfer.Move
        (DragEvent.dataTransfer dragEvt)
