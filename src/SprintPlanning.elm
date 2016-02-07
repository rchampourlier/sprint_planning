-- SprintPlanning is the top-level / application module.
module SprintPlanning where

import Effects exposing (Effects, Never)
import Http
import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Json.Decode exposing ((:=))
import Json.Decode as Json
import List
import Set
import Task

import Issue
import Issues
import TeamMember
import TeamMemberList


-- MODEL

type IssueStatus = TODO | DONE
type alias ID = Int
type alias Model =
  { issues : List Issue.Model
  , teamMemberList : TeamMemberList.Model
  , draggedTeamMemberName : Maybe String
  }

init : (Model, Effects Action)
init =
  ( Model [] (TeamMemberList.init []) Nothing
  , getIssues "PROJECT = \"JT\""
  )

getAssignments : List Issue.Model -> List String -> List (String, List (TeamMember.Role, Int))
getAssignments issues teamMemberNames =
  List.map (\name -> (name, getAssignmentsForName issues name)) teamMemberNames

getAssignmentsForName : List Issue.Model -> String -> List (TeamMember.Role, Int)
getAssignmentsForName issues name =
  List.map (\role -> (role, calculateAssignmentForNameAndRole issues name role)) [TeamMember.Developer, TeamMember.Reviewer]

calculateAssignmentForNameAndRole : List Issue.Model -> String -> TeamMember.Role -> Int
calculateAssignmentForNameAndRole issues teamMemberName role =
  let
    is : TeamMember.Role -> Issue.Model -> Bool
    is role issue =
      let maybeName =
        case role of
          TeamMember.Developer -> issue.developerName
          TeamMember.Reviewer -> issue.reviewerName
      in
        case maybeName of
          Nothing -> False
          Just name -> name == teamMemberName
  in
    List.filter (is role) issues
      |> sumEstimates

sumEstimates : List Issue.Model -> Int
sumEstimates issues =
  List.map (\i -> i.estimate) issues
    |> List.foldl (+) 0

getIssuesForStatus : IssueStatus -> Model -> List Issue.Model
getIssuesForStatus status model =
  case status of
    TODO -> List.filter (\i -> i.developerName == Nothing || i.reviewerName == Nothing) model.issues
    DONE -> List.filter (\i -> i.developerName /= Nothing && i.reviewerName /= Nothing) model.issues


-- UPDATE

type Action
  = ReceivedIssues (Maybe (List Issue.Model))
  | ModifyIssue Issue.Model Issue.Action
  | ModifyTeamMembers TeamMemberList.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    ReceivedIssues maybeIssues ->
      case maybeIssues of
        Nothing -> ( model, Effects.none )
        Just issues ->
          let
            teamMemberList = Issues.teamMembersNames issues
              |> Set.toList
              |> TeamMemberList.init
          in
            ( updateWithIssues model issues
            , Effects.none )

    ModifyIssue issue issueAction ->
      let
        updateIssue i =
          if i == issue
            then Issue.update issueAction issue model.draggedTeamMemberName
            else i
        issues = List.map updateIssue model.issues
      in
        ( updateWithIssues model issues
        , Effects.none )

    ModifyTeamMembers teamMemberListAction ->
      ( { model
          | teamMemberList = TeamMemberList.update teamMemberListAction model.teamMemberList
        }
      , Effects.none )

updateWithIssues : Model -> List Issue.Model -> Model
updateWithIssues model issues =
  let
    teamMemberList = case model.teamMemberList of
      [] ->
        Issues.teamMembersNames issues
          |> Set.toList
          |> TeamMemberList.init
      _ -> model.teamMemberList
    teamMemberNames = TeamMemberList.getNames teamMemberList
    updatedTeamMemberList = TeamMemberList.updateAssignments teamMemberList (getAssignments issues teamMemberNames)
  in
    { model
      | issues = issues
      , teamMemberList = updatedTeamMemberList
    }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    issuesTodo = getIssuesForStatus TODO model
    issuesDone = getIssuesForStatus DONE model
    teamMemberNames = TeamMemberList.getNames model.teamMemberList
  in
    div [ class "sprint-planning" ]
      [ div [ class "issues-box box mui-panel mui-col-md-9" ]
        [ h2 [] [ text "Issues" ]
        , div [] [ viewIssues address "TODO" issuesTodo teamMemberNames ]
        , div [] [ viewIssues address "DONE" issuesDone teamMemberNames ]
        ]
      , div [ class "team-members-box box mui-panel mui-col-md-3" ]
        [ h2 [] [ text "Team Members" ]
        , div [] [ viewTeamMembers address model ]
        ]
      ]

viewIssues : Signal.Address Action -> String -> List Issue.Model -> List String -> Html
viewIssues address label issues teamMemberNames =
  let
    viewIssue : Issue.Model -> Html
    viewIssue issue = Issue.view (Signal.forwardTo address (ModifyIssue issue)) issue teamMemberNames
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
  TeamMemberList.view (Signal.forwardTo address ModifyTeamMembers) model.teamMemberList


-- EFFECTS

getIssues : String -> Effects Action
getIssues jqlQuery =
  let
    url_base = "http://api.sprint-planning.dev/issues"
    url = Http.url url_base [ ]
  in
    Http.get (Json.list decodeIssue) url
      |> Task.toMaybe
      |> Task.map ReceivedIssues
      |> Effects.task

-- We must beware that if the decoder fails to decode
-- a value (for example an estimate is null, and not
-- an int), the whole decoder will return Nothing
-- without more warning.
decodeIssue : Json.Decoder Issue.Model
decodeIssue =
  Json.object5 Issue.init
    ("key" := Json.string)
    ("summary" := Json.string)
    (Json.oneOf [ "estimate" := Json.int, Json.succeed 0 ])
    (Json.maybe ( "developer" := Json.string ))
    (Json.maybe ( "reviewer" := Json.string ))
