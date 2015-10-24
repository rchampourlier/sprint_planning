module TeamMember where

import Html exposing (..)

import Issue


-- MODEL

type alias Model =
  { capacity : Float
  , name : String
  }

init : String -> Float -> List Issue.Model -> Model
init name capacity assignedIssues =
  { capacity = capacity
  , name = name
  }


-- VIEW

view : Model -> Float -> List Html -> Html
view model assignedCapacity assignedIssueViews =
  div []
    [ div [] [ text model.name ]
    , div []
      [ span [] [ text "Initial capacity: " ]
      , span [] [ text <| toString model.capacity ]
      ]
    , div []
      [ span [] [ text "Remaining: " ]
      , span [] [ text <| toString (model.capacity - assignedCapacity) ]
      ]
    , div []
      [ span [] [ text "Assigned issues: " ]
      , div [] assignedIssueViews
      ]
    ]
