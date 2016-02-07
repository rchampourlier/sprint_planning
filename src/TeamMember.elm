module TeamMember where

import Html exposing (..)
import IntegerInput


-- MODEL

type Role = Developer | Reviewer
type alias Model =
  { capacity : IntegerInput.Model
  , name : String
  , assignmentDeveloper : Int
  , assignmentReviewer : Int
  }

init : String -> Int -> Model
init name capacity =
  { capacity = IntegerInput.init capacity
  , name = name
  , assignmentDeveloper = 0
  , assignmentReviewer = 0
  }


-- UPDATE

type Action
  = ModifyCapacity IntegerInput.Action

update : Action -> Model -> Model
update action model =
  case action of
    ModifyCapacity integerInputAction ->
      { model | capacity = IntegerInput.update integerInputAction model.capacity }

updateAssignments : Model -> List (Role, Int) -> Model
updateAssignments model roleAssignments =
  let
    -- applyAssignment : (Role, Int) -> Model -> Model
    applyAssignment (role, assignment) model =
      case role of
        Developer -> { model | assignmentDeveloper = assignment }
        Reviewer -> { model | assignmentReviewer = assignment }
  in
    List.foldl applyAssignment model roleAssignments

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    remaining = (IntegerInput.getValue model.capacity) - model.assignmentDeveloper
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
        [ span [] [ text <| "Review: " ++ (toString model.assignmentReviewer) ] ]
      ]
