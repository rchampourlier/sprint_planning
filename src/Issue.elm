module Issue (Action, Model, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (class, key)
import MoreHtmlEvents exposing (onDrag, onDragOver, onDrop)

import Mui


-- MODEL

type Role = Developer | Reviewer

type alias Model =
  { key : String
  , summary : String
  , estimate : Int
  , developerName : Maybe String
  , reviewerName : Maybe String
  }

init : String -> String -> Int -> Maybe String -> Maybe String -> Model
init key summary estimate maybeDeveloperName maybeReviewerName =
  { key = key
  , summary = summary
  , estimate = estimate
  , developerName = maybeDeveloperName
  , reviewerName = maybeReviewerName
  }


-- UPDATE

type Action
  = DragOver Role
  | DropAndAssign Role
  | Assign Role Mui.Action
  | Unassign Role

update : Action -> Model -> Maybe String -> Model
update action model maybeTeamMemberName =
  case action of

    DragOver role -> model

    DropAndAssign role ->
      case role of
        Developer -> { model | developerName = maybeTeamMemberName }
        Reviewer -> { model | reviewerName = maybeTeamMemberName }

    Assign role muiAction ->
      let
        selectedTeamMemberName = Mui.selectedID muiAction
      in
        case role of
          Developer ->
            { model | developerName = selectedTeamMemberName }
          Reviewer ->
            { model | reviewerName = selectedTeamMemberName }

    Unassign role ->
      case role of
        Developer -> { model | developerName = Nothing }
        Reviewer -> { model | reviewerName = Nothing }


-- VIEW

view : Signal.Address Action -> Model -> List String -> Html
view address model teamMemberNames =
  let
    options = List.map (\n -> (n, n)) teamMemberNames
  in
    tr [ class "issue-item", key model.key ]
      [ td []
        [ strong [] [ text model.key ]
        , span [ class "issue-item__summary" ] [ text model.summary ]
        ]
      , td [] [ text <| toString model.estimate ]
      , td
        [ onDragOver address (DragOver Developer)
        , onDrop address (DropAndAssign Developer)
        ]
        [ Mui.selectBox (Signal.forwardTo address (Assign Developer)) "None" options model.developerName ]
      , td
        [ onDragOver address (DragOver Reviewer)
        , onDrop address (DropAndAssign Reviewer)
        ]
        [ Mui.selectBox (Signal.forwardTo address (Assign Reviewer)) "None" options model.reviewerName ]
      ]
