module TeamMember where

import Html exposing (..)
import IntegerInput


-- MODEL

type alias Model =
  { capacity : IntegerInput.Model
  , name : String
  }

init : String -> Int -> Model
init name capacity =
  { capacity = IntegerInput.init capacity
  , name = name
  }


-- UPDATE

type Action
  = ModifyCapacity IntegerInput.Action

update : Action -> Model -> Model
update action model =
  case action of
    ModifyCapacity integerInputAction ->
      Debug.log "ModifyCapacity integerInputAction"
      { model | capacity = IntegerInput.update integerInputAction model.capacity }


-- VIEW

view : Signal.Address Action -> Model -> Int -> Int -> Html
view address model developerEstimate reviewerEstimate =
  let
    remaining = (IntegerInput.getValue model.capacity) - developerEstimate
    viewIntegerInput = IntegerInput.view (Signal.forwardTo address ModifyCapacity) model.capacity
  in
    div []
      [ h4 [] [ text model.name ]
      , div []
        [ span [] [ text "Capacity: " ]
        , span [] [ viewIntegerInput ]
        ]
      , div []
        [ span [] [ text "Remaining: " ]
        , span [] [ text <| toString remaining ]
        ]
      , div []
        [ span [] [ text <| "Review: " ++ (toString reviewerEstimate) ] ]
      ]
