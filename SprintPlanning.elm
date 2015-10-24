module SprintPlanning where

import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Html.Events exposing (onClick)
import DragAndDropEvents exposing (onDrag, onDragOver, onDrop)
import Debug exposing (log)

import Issue
import TeamMember
import List


-- MODEL

type alias TeamMemberID = Int
type alias Assignment = (TeamMemberID, Issue.Model)
type alias Model =
  { draggedIssue : Maybe Issue.Model
  , teamMembers : List (TeamMemberID, TeamMember.Model)
  , assignments : List Assignment
  , unassignedIssues : List Issue.Model
  }

init : List Issue.Model -> List Assignment -> List (TeamMemberID, TeamMember.Model) -> Model
init unassignedIssues assignments teamMembers =
  { draggedIssue = Nothing
  , assignments = []
  , unassignedIssues = unassignedIssues
  , teamMembers = teamMembers
  }

getAssignedIssues : Model -> TeamMemberID -> List Issue.Model
getAssignedIssues model teamMemberID =
  List.filter (\(tmID, _) -> tmID == teamMemberID) model.assignments
    |> List.map (\(_, i) -> i)

getTeamMember : Model -> TeamMemberID -> Maybe TeamMember.Model
getTeamMember model teamMemberID =
  List.filter (\(tmID, _) -> tmID == teamMemberID) model.teamMembers
    |> List.map (\(_, tm) -> tm)
    |> List.head

-- UPDATE

type Action
  = Drag Issue.Model
  | DragOver
  | DropAndAssign TeamMemberID
  | DropAndUnassign

update : Action -> Model -> Model
update action model =
  case action of

    DropAndAssign teamMemberID ->
      case model.draggedIssue of
        Nothing -> model
        Just draggedIssue ->
          { model | assignments <- updateAssignments teamMemberID draggedIssue model.assignments,
                    unassignedIssues <- removeIssue draggedIssue model.unassignedIssues }

    DropAndUnassign ->
      case model.draggedIssue of
        Nothing -> model
        Just draggedIssue ->
          { model | assignments <- removeAssignment draggedIssue model.assignments,
                    unassignedIssues <- model.unassignedIssues ++ [ draggedIssue ] }

    Drag issue -> { model | draggedIssue <- Just issue }

    DragOver -> model

updateAssignments : TeamMemberID -> Issue.Model -> List Assignment -> List Assignment
updateAssignments teamMemberID issue assignments =
  List.filter (\(tmId, i) -> i /= issue) assignments
    |> List.append [ (teamMemberID, issue) ]

removeIssue : Issue.Model -> List Issue.Model -> List Issue.Model
removeIssue removedIssue issues =
  List.filter (\i -> i /= removedIssue) issues

removeAssignment : Issue.Model -> List Assignment -> List Assignment
removeAssignment unassignedIssue assignments =
  List.filter (\(_, i) -> (i /= unassignedIssue)) assignments


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    isDraggedIssue : Issue.Model -> (Issue.Model, Bool)
    isDraggedIssue issue =
      case model.draggedIssue of
        Nothing -> (issue, False)
        Just draggedIssue -> (issue, draggedIssue == issue)
    teamMemberViews = List.map (viewTeamMember address model) model.teamMembers
    unassignedIssueViews = List.map (viewUnassignedIssue address) (List.map isDraggedIssue model.unassignedIssues)
  in
    div []
      [ span [ style [ ("font-weight", "bold") ] ] [ text "Team members: " ]
      , div [ class "team-members-box" ] teamMemberViews
      , div
        [ class "unassigned-issues-box"
        , onDragOver address DragOver
        , onDrop address DropAndUnassign
        ]
        ([ span
          [ style [ ("font-weight", "bold") ] ]
          [ text "Unassigned issues: " ]
        ] ++ unassignedIssueViews)
      ]

-- Displays a team member (using TeamMember.view).
viewTeamMember : Signal.Address Action -> Model -> (TeamMemberID, TeamMember.Model) -> Html
viewTeamMember address model (teamMemberID, teamMember) =
  let
    assignedIssues = getAssignedIssues model teamMemberID
    assignedCapacity = List.map (\i -> i.estimate) assignedIssues |> List.foldl (+) 0
    assignedIssueViews = List.map (viewAssignedIssue address) assignedIssues
  in
    div
      [ class "team-member"
      , onDragOver address DragOver
      , onDrop address (DropAndAssign teamMemberID)
      ]
      [ TeamMember.view teamMember assignedCapacity assignedIssueViews
      ]

viewUnassignedIssue : Signal.Address Action -> (Issue.Model, Bool) -> Html
viewUnassignedIssue address (issue, beingDragged) =
  let
    htmlClasses = if beingDragged then "unassigned-issue" else "unassigned-issue dragged"
  in
    div
      [ draggable "true"
      , class htmlClasses
      , onDrag address (Drag issue)
      ]
      [ Issue.view issue
      ]

viewAssignedIssue : Signal.Address Action -> Issue.Model -> Html
viewAssignedIssue address issue =
  div
    [ draggable "true"
    , class ""
    , onDrag address (Drag issue)
    ]
    [ Issue.view issue
    ]
