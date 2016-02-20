module TeamMemberList where

import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Html.Events exposing (onClick)
import List
import ListFunctions exposing (indexList)

import IndexedList
import ProgressBar
import TeamMember


-- MODEL

type alias ID = Int
type alias Model = List (ID, TeamMember.Model)

init : List String -> Model
init names =
  List.map (\name -> TeamMember.init name 0) names
    |> indexList 0

getNames : Model -> List String
getNames model =
  model
    |> List.map (\(id, tm) -> tm)
    |> List.map (\tm -> TeamMember.getName tm)

getMaxCapacity : Model -> Int
getMaxCapacity model =
  List.map (\(id, tm) -> TeamMember.getCapacity tm) model
    |> List.maximum
    |> Maybe.withDefault 0


-- UPDATE

type Action
  = Add
  | Remove ID
  | Modify ID TeamMember.Action
  -- | Drag ID

update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      IndexedList.append model (TeamMember.init "Unknown" 0)
    Remove id -> model
    Modify id teamMemberAction ->
      let
        updateTeamMember : (ID, TeamMember.Model) -> (ID, TeamMember.Model)
        updateTeamMember (teamMemberID, teamMemberModel) =
          if teamMemberID == id then
            (teamMemberID, TeamMember.update teamMemberAction teamMemberModel)
          else
            (teamMemberID, teamMemberModel)
      in
        List.map updateTeamMember model

updateAssignments : Model -> List (String, List (TeamMember.Role, Int)) -> Model
updateAssignments model assignments =
  let
    applyAssignments : (String, List (TeamMember.Role, Int)) -> Model -> Model
    applyAssignments (name, nameAssignments) model =
      let
        applyIfMatchingName : (ID, TeamMember.Model) -> (ID, TeamMember.Model)
        applyIfMatchingName (id, teamMemberModel) =
          if TeamMember.getName teamMemberModel == name
            then (id, TeamMember.updateAssignments teamMemberModel nameAssignments)
            else (id, teamMemberModel)
      in
        List.map applyIfMatchingName model
  in
    List.foldl applyAssignments model assignments

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    maxCapacity = toFloat <| getMaxCapacity model
    viewButtonAdd = button
      [ class "mui-btn mui-btn--primary", onClick address Add ]
      [ text "Add" ]
    viewList = viewTeamMemberList address model maxCapacity
  in
    div []
      [ table [ class "team-members-list" ] viewList
      , viewButtonAdd
      ]

viewTeamMemberList : Signal.Address Action -> Model -> Float -> List Html
viewTeamMemberList address model maxCapacity =
  List.concatMap (viewTeamMember address maxCapacity) model

viewTeamMember : Signal.Address Action -> Float -> (ID, TeamMember.Model) -> List Html
viewTeamMember address maxCapacity (id, tm) =
  let
    assigned = TeamMember.getAssigned tm
    capacity = TeamMember.getCapacity tm
    remaining = (toFloat capacity) - assigned
    remainingPercentage = 100 * remaining / maxCapacity
  in
    [ TeamMember.view (Signal.forwardTo address (Modify id)) tm
    , ProgressBar.view remaining
    ]
