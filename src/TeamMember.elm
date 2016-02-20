module TeamMember where

import Html exposing (..)
import Html.Attributes exposing (class)
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

getAssigned : Model -> Float
getAssigned model =
  (toFloat model.assignmentDeveloper) + 0.2 * (toFloat model.assignmentReviewer)

getCapacity : Model -> Int
getCapacity model =
  IntegerInput.getValue model.capacity

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
    viewNameInput = StringInput.view (Signal.forwardTo address ModifyName) model.name
    viewCapacityInput = IntegerInput.view (Signal.forwardTo address ModifyCapacity) model.capacity
    viewAssigned = span [] [ text (toString assigned) ]
    assigned = getAssigned model
    remaining = toFloat (IntegerInput.getValue model.capacity) - assigned
    capacityProgress = 1 - remaining / toFloat (getCapacity model)
  in
    tr [ class "team-members-list__item" ]
      [ td
        [ class "team-members-list__item__segment team-members-list__item__segment--name" ]
        [ viewNameInput ]
      , td
        [ class "team-members-list__item__segment team-members-list__item__segment--capacity" ]
        [ viewCapacityInput ]
      , td
        [ class "team-members-list__item__segment team-members-list__item__segment--assigned" ]
        [ viewAssigned ]
      ]
