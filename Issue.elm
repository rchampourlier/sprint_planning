module Issue (Action, Model, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import DragAndDropEvents exposing (onDrag, onDragOver, onDrop)

import TeamMember


-- MODEL

type Role = Developer | Reviewer

type alias Model =
  { title : String
  , estimate : Float
  , developer : Maybe TeamMember.Model
  , reviewer : Maybe TeamMember.Model
  }

init : String -> Float -> Model
init title estimate =
  { title = title
  , estimate = estimate
  , developer = Nothing
  , reviewer = Nothing
  }


-- UPDATE

type Action
  = DragOver Role
  | DropAndAssign Role
  | Unassign Role

update : Action -> Model -> TeamMember.Model -> Model
update action model teamMember =
  case action of
    DragOver role -> model

    DropAndAssign role ->
      case role of
        Developer -> { model | developer = Just teamMember }
        Reviewer -> { model | reviewer = Just teamMember }

    Unassign role ->
      case role of
        Developer -> { model | developer = Nothing }
        Reviewer -> { model | reviewer = Nothing }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "issue-item card" ]
    [ h4 [] [ text model.title ]
    , div []
      [ span [] [ text "Estimate: " ]
      , span [] [ text <| toString model.estimate ]
      , div
        [ onDragOver address (DragOver Developer)
        , onDrop address (DropAndAssign Developer)
        ]
        [ span [] [ text "Developer: ", roleBox address Developer model ] ]
      , div
        [ onDragOver address (DragOver Reviewer)
        , onDrop address (DropAndAssign Reviewer)
        ]
        [ span [] [ text "Reviewer: ", roleBox address Reviewer model ] ]
      ]
    ]

roleBox : Signal.Address Action -> Role -> Model -> Html
roleBox address role model =
  let
    teamMember =
      case role of
        Developer -> model.developer
        Reviewer -> model.reviewer
  in
    case teamMember of
      Nothing -> span [] []
      Just tm ->
        button
          [ class "mui-btn"
          , onClick address (Unassign role)
          ]
        [ text tm.name ]
