module TeamMember where

import Html exposing (..)
import IntegerInput
import StringInput


-- MODEL

type Role = Developer | Reviewer
type alias Model =
  { capacity : IntegerInput.Model
  , name : StringInput.Model
  , assignmentDeveloper : Int
  , assignmentReviewer : Int
  }

init : String -> Int -> Model
init name capacity =
  { capacity = IntegerInput.init capacity
  , name = StringInput.init name
  , assignmentDeveloper = 0
  , assignmentReviewer = 0
  }

getName : Model -> String
getName model =
  StringInput.getValue model.name


-- UPDATE

type Action
  = ModifyCapacity IntegerInput.Action
  | ModifyName StringInput.Action

update : Action -> Model -> Model
update action model =
  case action of
    ModifyCapacity integerInputAction ->
      { model | capacity = IntegerInput.update integerInputAction model.capacity }
    ModifyName stringInputAction ->
      { model | name = StringInput.update stringInputAction model.name }

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
    viewCapacityInput = IntegerInput.view (Signal.forwardTo address ModifyCapacity) model.capacity
    viewNameInput = StringInput.view (Signal.forwardTo address ModifyName) model.name
  in
    div []
      [ h4 [] [ viewNameInput ]
      , div []
        [ span [] [ text "Capacity: " ]
        , span [] [ viewCapacityInput ]
        ]
      , div []
        [ span [] [ text "Remaining: " ]
        , span [] [ text <| toString remaining ]
        ]
      , div []
        [ span [] [ text <| "Review: " ++ (toString model.assignmentReviewer) ] ]
      ]
