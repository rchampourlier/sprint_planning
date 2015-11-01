module SprintPlanning where

import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Html.Events exposing (onClick)
import DragAndDropEvents exposing (onDrag, onDragOver, onDrop)
import Debug exposing (log)

import Issue
import TeamMember
import List
import ListFunctions


-- MODEL

type Role = Developer | Reviewer
type alias Assignment = (TeamMember.Model, Role, Issue.Model)
type alias Model =
  { issues : List Issue.Model
  , teamMembers : List TeamMember.Model
  , draggedTeamMember : Maybe TeamMember.Model
  , assignments : List Assignment
  }

init : List Issue.Model -> List TeamMember.Model -> Model
init issues teamMembers =
  { issues = issues
  , teamMembers = teamMembers
  , draggedTeamMember = Nothing
  , assignments = []
  }

getAssignmentsTotalEstimate : Model -> TeamMember.Model -> Role -> Float
getAssignmentsTotalEstimate model teamMember role =
  List.filter (\(tm, r, _) -> tm == teamMember && r == role) model.assignments
    |> List.map issueFromAssignment
    |> sumEstimates

getIssuesWithoutDeveloper : Model -> List Issue.Model
getIssuesWithoutDeveloper model =
  let issues =
    List.filter (\(_, r, _) -> r == Developer) model.assignments
      |> List.map issueFromAssignment
  in
    List.filter (\i -> not (List.member i issues)) model.issues

getIssuesWithDeveloperNoReviewer : Model -> List Issue.Model
getIssuesWithDeveloperNoReviewer model =
  let
    issuesWithoutDeveloper = getIssuesWithoutDeveloper model
    issuesOk = getIssuesOk model
  in
    ListFunctions.substract model.issues (issuesOk ++ issuesWithoutDeveloper)

getIssuesOk : Model -> List Issue.Model
getIssuesOk model = List.filter (issueOk model.assignments) model.issues

getIssueAssignments : Model -> Issue.Model -> List Assignment
getIssueAssignments model issue =
  filterAssignmentsForIssue issue model.assignments

issueFromAssignment : Assignment -> Issue.Model
issueFromAssignment (_, _, i) = i

filterAssignmentsForIssue : Issue.Model -> List Assignment -> List Assignment
filterAssignmentsForIssue issue assignments =
  List.filter (\(_, _, i) -> i == issue) assignments

filterAssignmentsForRole : Role -> List Assignment -> List Assignment
filterAssignmentsForRole role assignments =
  List.filter (\(_, r, _) -> r == role) assignments

issueOk : List Assignment -> Issue.Model -> Bool
issueOk assignments issue =
  List.length (filterAssignmentsForIssue issue assignments) == 2

sumEstimates : List Issue.Model -> Float
sumEstimates issues =
  List.map (\i -> i.estimate) issues
    |> List.foldl (+) 0


-- UPDATE

type Action
  = Drag TeamMember.Model
  | DragOver
  | DropAndAssign Issue.Model Role

update : Action -> Model -> Model
update action model =
  case action of

    DropAndAssign issue role ->
      case model.draggedTeamMember of
        Nothing -> model
        Just draggedTeamMember ->
          { model |
              assignments <- updateAssignments draggedTeamMember issue role model.assignments
          }

    Drag teamMember -> { model | draggedTeamMember <- Just teamMember }
    DragOver -> model

updateAssignments : TeamMember.Model -> Issue.Model -> Role -> List Assignment -> List Assignment
updateAssignments teamMember issue role assignments =
  removeAssignment issue role assignments
    |> List.append [ (teamMember, role, issue) ]

removeAssignment : Issue.Model -> Role -> List Assignment -> List Assignment
removeAssignment issue role assignments =
  List.filter (\(_, r, i) -> (r /= role || i /= issue)) assignments


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [ class "box" ]
      [ span [] [ text "Issues" ]
      , div [] [ viewIssues address model "Without developer" getIssuesWithoutDeveloper ]
      , div [] [ viewIssues address model "Without reviewer" getIssuesWithDeveloperNoReviewer ]
      , div [] [ viewIssues address model "Fully assigned" getIssuesOk ]
      ]
    , div [ class "box" ]
      [ span [] [ text "Team Members" ]
      , div [] [ viewTeamMembers address model ]
      ]
    ]

viewIssues : Signal.Address Action -> Model -> String -> (Model -> List Issue.Model) -> Html
viewIssues address model label getIssues =
  div []
    [ span [] [ text label ]
    , div [] (List.map (viewIssueAssignable address model) (getIssues model))
    ]

viewIssueAssignable : Signal.Address Action -> Model -> Issue.Model -> Html
viewIssueAssignable address model issue =
  let
    roleLabel : Role -> String
    roleLabel role =
      let
        assignment =
          filterAssignmentsForRole role (getIssueAssignments model issue)
            |> List.head
      in
        case assignment of
          Nothing -> "?"
          Just (tm, _, _) -> tm.name
  in
    div [ class "box" ]
      [ Issue.view issue
      , div
        [ onDragOver address DragOver
        , onDrop address (DropAndAssign issue Developer)
        ]
        [ span [ class "box" ] [ text <| "Developer: " ++ (roleLabel Developer) ] ]
      , div
        [ onDragOver address DragOver
        , onDrop address (DropAndAssign issue Reviewer)
        ]
        [ span [ class "box" ] [ text <| "Reviewer: " ++ (roleLabel Reviewer) ] ]
      ]

viewTeamMembers : Signal.Address Action -> Model -> Html
viewTeamMembers address model =
  div []
    (List.map (viewTeamMemberDraggable address model) model.teamMembers)

viewTeamMemberDraggable : Signal.Address Action -> Model -> TeamMember.Model -> Html
viewTeamMemberDraggable address model teamMember =
  let
    developerEstimate = getAssignmentsTotalEstimate model teamMember Developer
    reviewerEstimate = getAssignmentsTotalEstimate model teamMember Reviewer
  in
    div
      [ class "box"
      , draggable "true"
      , onDrag address (Drag teamMember)
      ]
      [ TeamMember.view teamMember developerEstimate reviewerEstimate ]
