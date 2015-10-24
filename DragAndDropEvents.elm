module DragAndDropEvents where

import Html.Events exposing (on, onWithOptions)
import Html exposing (Attribute)
import Json.Decode as Json exposing (..)

onDrag : Signal.Address a -> a -> Attribute
onDrag addr message =
  on "drag" value (\_ -> Signal.message addr message)

onDragOver : Signal.Address a -> a -> Attribute
onDragOver addr message =
  let options = { stopPropagation = True, preventDefault = True }
  in onWithOptions "dragover" options value (\_ -> Signal.message addr message)

onDrop : Signal.Address a -> a -> Attribute
onDrop addr message =
  on "drop" value (\_ -> Signal.message addr message)
