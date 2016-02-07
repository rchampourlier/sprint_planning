module TeamMemberList where

import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import List
import ListFunctions exposing (indexList)

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
    |> List.map (\tm -> tm.name)


-- UPDATE

type Action
  = Add
  | Remove
  | Modify ID TeamMember.Action
  -- | Drag ID

update : Action -> Model -> Model
update action model =
  case action of
    Add -> model
    Remove -> model
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
          if teamMemberModel.name == name
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
    viewTeamMember : (ID, TeamMember.Model) -> Html
    viewTeamMember (id, tm) =
      viewTeamMemberDraggable address (id, tm)
  in
    div [ ] (List.map viewTeamMember model)

viewTeamMemberDraggable : Signal.Address Action -> (ID, TeamMember.Model) -> Html
viewTeamMemberDraggable address (id, tm) =
  div
    [ class "team-member-item card"
    , draggable "true"
    -- , onDrag (Signal.forwardTo address (Drag id))
    ]
    [ TeamMember.view (Signal.forwardTo address (Modify id)) tm ]
