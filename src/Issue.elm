module Issue (Action, Model, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (class, key)
import Html.Events exposing (onClick)
import MoreHtmlEvents exposing (onDrag, onDragOver, onDrop)
import Debug exposing (log)

import Mui

import TeamMember


-- MODEL

type alias TeamMemberID = Int
type Role = Developer | Reviewer

type alias Model =
  { key : String
  , summary : String
  , estimate : Float
  , developerID : Maybe TeamMemberID
  , reviewerID : Maybe TeamMemberID
  }

init : String -> String -> Float -> Model
init key summary estimate =
  { key = key
  , summary = summary
  , estimate = estimate
  , developerID = Nothing
  , reviewerID = Nothing
  }


-- UPDATE

type Action
  = DragOver Role
  | DropAndAssign Role
  | Assign Role Mui.Action
  | Unassign Role

update : Action -> Model -> Maybe TeamMemberID -> Model
update action model maybeTeamMemberID =
  case action of

    DragOver role -> model

    DropAndAssign role ->
      case role of
        Developer -> { model | developerID = maybeTeamMemberID }
        Reviewer -> { model | reviewerID = maybeTeamMemberID }

    Assign role muiAction ->
      let
        selectedTeamMemberID = Mui.selectedID muiAction
      in
        case role of
          Developer ->
            { model | developerID = selectedTeamMemberID }
          Reviewer ->
            { model | reviewerID = selectedTeamMemberID }

    Unassign role ->
      case role of
        Developer -> { model | developerID = Nothing }
        Reviewer -> { model | reviewerID = Nothing }


-- VIEW

view : Signal.Address Action -> Model -> List (TeamMemberID, String) -> Html
view address model teamMemberIDAndNames =
  tr [ class "issue-item", key model.key ]
    [ td []
      [ strong [] [ text model.key ]
      , span [] [ text model.summary ]
      ]
    , td [] [ text <| toString model.estimate ]
    , td
      [ onDragOver address (DragOver Developer)
      , onDrop address (DropAndAssign Developer)
      ]
      [ Mui.selectBox (Signal.forwardTo address (Assign Developer)) "None" teamMemberIDAndNames model.developerID ]
    , td
      [ onDragOver address (DragOver Reviewer)
      , onDrop address (DropAndAssign Reviewer)
      ]
      [ Mui.selectBox (Signal.forwardTo address (Assign Reviewer)) "None" teamMemberIDAndNames model.reviewerID ]
    ]
