module MoreHtmlEvents where

import Html.Events exposing (on, onWithOptions, targetValue)
import Html exposing (Attribute)
import Json.Decode

onDrag : Signal.Address a -> a -> Attribute
onDrag address message =
  on "drag" Json.Decode.value (\_ -> Signal.message address message)

onDragOver : Signal.Address a -> a -> Attribute
onDragOver address message =
  let options = { stopPropagation = True, preventDefault = True }
  in onWithOptions "dragover" options Json.Decode.value (\_ -> Signal.message address message)

onDrop : Signal.Address a -> a -> Attribute
onDrop address message =
  on "drop" Json.Decode.value (\_ -> Signal.message address message)

onChange : Signal.Address a -> (String -> a) -> Attribute
onChange address contentToValue =
  on "change" targetValue (\str -> Signal.message address (contentToValue str))
