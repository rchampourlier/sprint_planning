module SprintPlanning where

import Effects exposing (Effects, Never)
import Http
import Html exposing (..)
import Html.Attributes exposing (class, style, draggable)
import Json.Decode exposing ((:=))
import Json.Decode as Json
import MoreHtmlEvents exposing (onDrag, onDragOver, onDrop)
import List
import ListFunctions exposing (indexList)
import Task

import Issue
import Issues
import TeamMember
import TeamMembers


-- MODEL

type Role = Developer | Reviewer
type IssueStatus = TODO | DONE
type alias ID = Int
type alias Model =
  { issues : List Issue.Model
  , teamMembers : List (ID, TeamMember.Model)
  , draggedTeamMemberName : Maybe String
  }

init : (Model, Effects Action)
init =
  ( Model [] [] Nothing
  , getIssues "PROJECT = \"JT\""
  )

getTotalAssignedEstimate : Model -> String -> Role -> Int
getTotalAssignedEstimate model teamMemberName role =
  let
    is : Role -> String -> Issue.Model -> Bool
    is role teamMemberName issue =
      let issueTMNameForRole =
        case role of
          Developer -> issue.developerName
          Reviewer -> issue.reviewerName
      in
        case issueTMNameForRole of
          Nothing -> False
          Just tmName -> tmName == teamMemberName
  in
    List.filter (is role teamMemberName) model.issues
      |> sumEstimates

getIssuesForStatus : IssueStatus -> Model -> List Issue.Model
getIssuesForStatus status model =
  case status of
    TODO -> List.filter (\i -> i.developerName == Nothing || i.reviewerName == Nothing) model.issues
    DONE -> List.filter (\i -> i.developerName /= Nothing && i.reviewerName /= Nothing) model.issues

sumEstimates : List Issue.Model -> Int
sumEstimates issues =
  List.map (\i -> i.estimate) issues
    |> List.foldl (+) 0


-- UPDATE

type Action
  = ReceivedIssues (Maybe (List Issue.Model))
  | Drag String
  | ModifyIssue Issue.Model Issue.Action
  | ModifyTeamMember ID TeamMember.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    ReceivedIssues maybeIssues ->
      case maybeIssues of
        Nothing -> ( model, Effects.none )
        Just issues ->
          let
            names = Issues.teamMembersNames issues
            teamMemberModels = TeamMembers.fromNames names
          in
            ( { model | issues = issues, teamMembers = (indexList teamMemberModels 0) }
            , Effects.none )

    Drag teamMemberName ->
      ( { model | draggedTeamMemberName = Just teamMemberName }
      , Effects.none )

    ModifyIssue issue issueAction ->
      let updateIssue i =
        if i == issue
          then Issue.update issueAction issue model.draggedTeamMemberName
          else i
      in
        ( { model | issues = List.map updateIssue model.issues }
        , Effects.none )

    ModifyTeamMember id teamMemberAction ->
      let
        updateTeamMember : (ID, TeamMember.Model) -> (ID, TeamMember.Model)
        updateTeamMember (teamMemberID, teamMemberModel) =
          if teamMemberID == id then
            (teamMemberID, TeamMember.update teamMemberAction teamMemberModel)
          else
            (teamMemberID, teamMemberModel)
      in
        ( { model | teamMembers = List.map updateTeamMember model.teamMembers }
        , Effects.none )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    issuesTodo = getIssuesForStatus TODO model
    issuesDone = getIssuesForStatus DONE model
  in
    div [ class "sprint-planning" ]
      [ div [ class "issues-box box mui-panel mui-col-md-9" ]
        [ h2 [] [ text "Issues" ]
        , div [] [ viewIssues address "TODO" issuesTodo model.teamMembers ]
        , div [] [ viewIssues address "DONE" issuesDone model.teamMembers ]
        ]
      , div [ class "team-members-box box mui-panel mui-col-md-3" ]
        [ h2 [] [ text "Team Members" ]
        , div [] [ viewTeamMembers address model ]
        ]
      ]

viewIssues : Signal.Address Action -> String -> List Issue.Model -> List (ID, TeamMember.Model) -> Html
viewIssues address label issues teamMembersWithIDs =
  let
    teamMemberNames = teamMembersWithIDs
      |> List.map (\(id, tm) -> tm)
      |> TeamMembers.toNames
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
  div
    []
    (List.map (viewTeamMemberDraggable address model) model.teamMembers)

viewTeamMemberDraggable : Signal.Address Action -> Model -> (ID, TeamMember.Model) -> Html
viewTeamMemberDraggable address model (teamMemberID, teamMemberModel) =
  let
    developerEstimate = getTotalAssignedEstimate model teamMemberModel.name Developer
    reviewerEstimate = getTotalAssignedEstimate model teamMemberModel.name Reviewer
    viewTeamMember =
      TeamMember.view (Signal.forwardTo address (ModifyTeamMember teamMemberID)) teamMemberModel developerEstimate reviewerEstimate
  in
    div
      [ class "team-member-item card"
      , draggable "true"
      , onDrag address (Drag teamMemberModel.name)
      ]
      [ viewTeamMember ]


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
