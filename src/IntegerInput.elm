module IntegerInput where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onKeyPress, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (toInt)


-- MAIN
-- This can be used to test the component isolated
-- using elm-reactor.

main : Signal Html
main =
  StartApp.start
    { model = init 0
    , view = view
    , update = update
    }


-- MODEL

type State = Display | Edit
type alias Model =
  { value : Int
  , state : State
  }

init : Int -> Model
init value =
  Model value Display

getValue : Model -> Int
getValue model = model.value


-- UPDATE

type Action
  = DoNothing
  | EnterEdit
  | LeaveEdit
  | UpdateValue String

update : Action -> Model -> Model
update action model =
  case action of
    EnterEdit ->
      { model | state = Edit }
    LeaveEdit -> { model | state = Display }
    UpdateValue newValueStr ->
      let
        newValueResult = toInt newValueStr
      in
        case newValueResult of
          Ok newValueInt -> { model | value = newValueInt }
          Err newValueErr -> model
    DoNothing -> model


-- VIEW

view : Address Action -> Model -> Html
view address model =
  case model.state of
    Display -> viewDisplay address model
    Edit -> viewEdit address model

viewDisplay : Address Action -> Model -> Html
viewDisplay address model =
  span
    [ class "editable-label" ]
    [ text <| toString model.value
    , span
      [ onClick address EnterEdit
      , class "editable-label__button"
      ]
      [ text <| "edit" ]
    ]

viewEdit : Address Action -> Model -> Html
viewEdit address model =
  let
    actionForKey : Int -> Action
    actionForKey key =
      case key of
        13 -> LeaveEdit
        _ -> DoNothing
  in
    span [ ]
    [ input
      ( [ class "editable-label"
        , value (toString model.value)
        , on "input" targetValue (\str -> Signal.message address (UpdateValue str))
        , onKeyPress address actionForKey
        ]
      ) [ ]
    ]
