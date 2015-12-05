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
type IssueStatus = MissingDeveloper | MissingReviewer | Ok
type alias Assignment = (TeamMember.Model, Role, Issue.Model)
type alias Model =
  { issues : List Issue.Model
  , teamMembers : List TeamMember.Model
  , draggedTeamMember : Maybe TeamMember.Model
  }

init : List Issue.Model -> List TeamMember.Model -> Model
init issues teamMembers =
  { issues = issues
  , teamMembers = teamMembers
  , draggedTeamMember = Nothing
  }

getTotalAssignedEstimate : Model -> TeamMember.Model -> Role -> Float
getTotalAssignedEstimate model teamMember role =
  let
    is : Role -> TeamMember.Model -> Issue.Model -> Bool
    is role teamMember issue =
      let issueTMForRole =
        case role of
          Developer -> issue.developer
          Reviewer -> issue.reviewer
      in
        case issueTMForRole of
          Nothing -> False
          Just tm -> tm == teamMember
  in
    List.filter (is role teamMember) model.issues
      |> sumEstimates

getIssuesForStatus : IssueStatus -> Model -> List Issue.Model
getIssuesForStatus status model =
  case status of
    MissingDeveloper -> List.filter (\i -> i.developer == Nothing) model.issues
    MissingReviewer -> List.filter (\i -> i.developer /= Nothing && i.reviewer == Nothing) model.issues
    Ok -> List.filter (\i -> i.developer /= Nothing && i.reviewer /= Nothing) model.issues

sumEstimates : List Issue.Model -> Float
sumEstimates issues =
  List.map (\i -> i.estimate) issues
    |> List.foldl (+) 0


-- UPDATE

type Action
  = Drag TeamMember.Model
  | Modify Issue.Model Issue.Action

update : Action -> Model -> Model
update action model =
  case action of
    Drag teamMember ->
      ( { model | draggedTeamMember = Just teamMember }
      , Effects.none )
    Modify issue issueAction ->
      case model.draggedTeamMember of
        Nothing -> model
        Just draggedTM ->
          let updateIssue i =
            if i == issue
              then Issue.update issueAction issue draggedTM
              else i
          in
            ( { model | issues = List.map updateIssue model.issues }
            , Effects.none )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    issuesWithoutDeveloper = getIssuesForStatus MissingDeveloper model
    issuesWithoutReviewer = getIssuesForStatus MissingReviewer model
    issuesOk = getIssuesForStatus Ok model
  in
    div [ class "sprint-planning" ]
      [ div [ class "issues-box box" ]
        [ h2 [] [ text "Issues" ]
        , div [] [ viewIssues address "Without developer" issuesWithoutDeveloper ]
        , div [] [ viewIssues address "Without reviewer" issuesWithoutReviewer ]
        , div [] [ viewIssues address "Fully assigned" issuesOk ]
        ]
      , div [ class "team-members-box box" ]
        [ h2 [] [ text "Team Members" ]
        , div [] [ viewTeamMembers address model ]
        ]
      ]

viewIssues : Signal.Address Action -> String -> List Issue.Model -> Html
viewIssues address label issues =
  let
    viewIssue : Issue.Model -> Html
    viewIssue issue = Issue.view (Signal.forwardTo address (Modify issue)) issue
  in
    div []
      [ h3 [] [ text label ]
      , div [ class "issues-bucket" ] (List.map viewIssue issues)
      ]

viewTeamMembers : Signal.Address Action -> Model -> Html
viewTeamMembers address model =
  div []
    (List.map (viewTeamMemberDraggable address model) model.teamMembers)

viewTeamMemberDraggable : Signal.Address Action -> Model -> TeamMember.Model -> Html
viewTeamMemberDraggable address model teamMember =
  let
    developerEstimate = getTotalAssignedEstimate model teamMember Developer
    reviewerEstimate = getTotalAssignedEstimate model teamMember Reviewer
  in
    div
      [ class "team-member-item card"
      , draggable "true"
      , onDrag address (Drag teamMember)
      ]
      [ TeamMember.view teamMember developerEstimate reviewerEstimate ]
