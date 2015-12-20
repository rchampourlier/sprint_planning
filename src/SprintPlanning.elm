module SprintPlanning where

import Effects exposing (Effects, Never)
import Http
import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Html.Events exposing (onClick)
import MoreHtmlEvents exposing (onDrag, onDragOver, onDrop)
import List
import ListFunctions
import Json.Decode as Json
import Task
import Debug exposing (log)

import Issue
import TeamMember


-- MODEL

type alias TeamMemberID = Int
type Role = Developer | Reviewer
type IssueStatus = TODO | DONE
type alias Assignment = (TeamMember.Model, Role, Issue.Model)
type alias Model =
  { issues : List Issue.Model
  , teamMembers : List (TeamMemberID, TeamMember.Model)
  , draggedTeamMemberID : Maybe TeamMemberID
  }

init : List Issue.Model -> List TeamMember.Model -> (Model, Effects Action)
init issues teamMembers =
  let
    indexList : List a -> Int -> List (Int, a)
    indexList list startIndex =
      case list of
        head :: tail -> (startIndex, head) :: indexList tail (startIndex + 1)
        _ -> []
    teamMembersWithIDs = indexList teamMembers 0
  in
    ( Model issues teamMembersWithIDs Nothing
    , getIssues "PROJECT = \"JT\""
    )

getTotalAssignedEstimate : Model -> TeamMemberID -> Role -> Float
getTotalAssignedEstimate model teamMemberID role =
  let
    is : Role -> TeamMemberID -> Issue.Model -> Bool
    is role teamMemberID issue =
      let issueTMIDForRole =
        case role of
          Developer -> issue.developerID
          Reviewer -> issue.reviewerID
      in
        case issueTMIDForRole of
          Nothing -> False
          Just tmID -> tmID == teamMemberID
  in
    List.filter (is role teamMemberID) model.issues
      |> sumEstimates

getIssuesForStatus : IssueStatus -> Model -> List Issue.Model
getIssuesForStatus status model =
  case status of
    TODO -> List.filter (\i -> i.developerID == Nothing || i.reviewerID == Nothing) model.issues
    DONE -> List.filter (\i -> i.developerID /= Nothing && i.reviewerID /= Nothing) model.issues

sumEstimates : List Issue.Model -> Float
sumEstimates issues =
  List.map (\i -> i.estimate) issues
    |> List.foldl (+) 0


-- UPDATE

type Action
  = ReceivedIssues (Maybe String)
  | Drag TeamMemberID
  | Modify Issue.Model Issue.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    ReceivedIssues string ->
      ( model, Effects.none )

    Drag teamMemberID ->
      ( { model | draggedTeamMemberID = Just teamMemberID }
      , Effects.none )

    Modify issue issueAction ->
      let updateIssue i =
        if i == issue
          then Issue.update issueAction issue model.draggedTeamMemberID
          else i
      in
        ( { model | issues = List.map updateIssue model.issues }
        , Effects.none )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    issuesTodo = getIssuesForStatus TODO model
    issuesDone = getIssuesForStatus DONE model
    teamMemberIDAndNames = List.map (\(id, tm) -> (id, tm.name)) model.teamMembers
  in
    div [ class "sprint-planning" ]
      [ div [ class "issues-box box mui-panel mui-col-md-9" ]
        [ h2 [] [ text "Issues" ]
        , div [] [ viewIssues address "TODO" issuesTodo teamMemberIDAndNames ]
        , div [] [ viewIssues address "DONE" issuesDone teamMemberIDAndNames ]
        ]
      , div [ class "team-members-box box mui-panel mui-col-md-3" ]
        [ h2 [] [ text "Team Members" ]
        , div [] [ viewTeamMembers address model ]
        ]
      ]

viewIssues : Signal.Address Action -> String -> List Issue.Model -> List (TeamMemberID, String) -> Html
viewIssues address label issues teamMemberIDAndNames =
  let
    viewIssue : Issue.Model -> Html
    viewIssue issue = Issue.view (Signal.forwardTo address (Modify issue)) issue teamMemberIDAndNames
  in
    div []
      [ h3 [] [ text label ]
      , table [ class "mui-table issues-list" ]
        [ thead []
          [ tr []
            [ th [] [ text "Issue" ]
            , th [] [ text "Estimate" ]
            , th [] [ text "Developer" ]
            , th [] [ text "Reviewer" ]
            ]
          ]
        , tbody [] (List.map viewIssue issues)
        ]
      ]

viewTeamMembers : Signal.Address Action -> Model -> Html
viewTeamMembers address model =
  div []
    (List.map (viewTeamMemberDraggable address model) model.teamMembers)

viewTeamMemberDraggable : Signal.Address Action -> Model -> (TeamMemberID, TeamMember.Model) -> Html
viewTeamMemberDraggable address model (teamMemberID, teamMember) =
  let
    developerEstimate = getTotalAssignedEstimate model teamMemberID Developer
    reviewerEstimate = getTotalAssignedEstimate model teamMemberID Reviewer
  in
    div
      [ class "team-member-item card"
      , draggable "true"
      , onDrag address (Drag teamMemberID)
      ]
      [ TeamMember.view teamMember developerEstimate reviewerEstimate ]


-- EFFECTS

(=>) = (,)

getIssues : String -> Effects Action
getIssues jqlQuery =
  let
    url_base = "https://job_tomate:jwKFhx7hUqFFAp2ciKagdofhzXkqgCbKuhZVCYeH3GddkNPtLa@jobteaser.atlassian.net/rest/api/2/search/"
    url = Http.url url_base [ "jql" => jqlQuery ]
  in
    Http.get decodeUrl url
      |> Task.toMaybe
      |> Task.map ReceivedIssues
      |> Effects.task

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["total"] Json.string
