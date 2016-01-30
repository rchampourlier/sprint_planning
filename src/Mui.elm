module Mui (Action, selectBox, selectedID) where

import Html exposing (..)
import Html.Attributes exposing (attribute, property, class)
import MoreHtmlEvents exposing (onChange)
import Debug exposing (log)

type alias ID = String
type alias Option = (ID, String)


selectBox : Signal.Address Action -> String -> List Option -> Maybe ID -> Html
selectBox address blankLabel options maybeSelectedOptionID =
  let
    model = init blankLabel options maybeSelectedOptionID
  in
    view address model


-- MODEL

type alias Model =
  { blankLabel : String
  , options : List Option
  , selectedOptionID : Maybe ID
  }

init : String -> List Option -> Maybe ID -> Model
init blankLabel options maybeSelectedOptionID =
  { blankLabel = blankLabel
  , options = options
  , selectedOptionID = maybeSelectedOptionID
  }

idForValue : String -> Model -> Maybe ID
idForValue value model =
  let
    matchValue : Option -> Bool
    matchValue option = snd option == value
    matchingOption = List.head <| List.filter matchValue model.options
  in
    case matchingOption of
      Nothing ->
        log "idForValue -> Nothing"
        Nothing
      Just option ->
        log ("idForValue -> " ++ (toString option))
        Just (fst option)


-- UPDATE

type Action
  = SelectedOption (Maybe ID)

valueToAction : Model -> String -> Action
valueToAction model value =
  SelectedOption (idForValue value model)

selectedID : Action -> Maybe ID
selectedID action =
  case action of
    SelectedOption selectedID -> selectedID


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    baseViewOptions = List.map (viewOption model) model.options
    viewOptions = [ viewBlankOption model ] ++ baseViewOptions
  in
    div
      [ class "mui-select", onChange address (valueToAction model) ]
      [ select [] viewOptions ]

viewBlankOption : Model -> Html
viewBlankOption model =
  case model.selectedOptionID of
    Nothing -> viewOptionWithAttributes model.blankLabel True
    Just _ -> viewOptionWithAttributes model.blankLabel False

viewOption : Model -> Option -> Html
viewOption model (optionID, optionText) =
  case model.selectedOptionID of
    Nothing -> viewOptionWithAttributes optionText False
    Just selectedOptionID ->
      if selectedOptionID == optionID
        then viewOptionWithAttributes optionText True
        else viewOptionWithAttributes optionText False

viewOptionWithAttributes : String -> Bool -> Html
viewOptionWithAttributes optionText isSelected =
  -- Could not make it work using "property" instead of "attribute"
  if isSelected
    then node "option" [ attribute "selected" "selected" ] [ text optionText ]
    else node "option" [] [ text optionText ]
