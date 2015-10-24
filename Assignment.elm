module Assignment where

import Html exposing (..)
import Html.Attributes exposing (style)

import Issue as I
import TeamMember as TM


-- MODEL

type alias Model =
  { assignee : TM.Model
  , issue : I.Model
  }

init : TM.Model -> I.Model -> Model
init teamMember issue =
  { assignee = teamMember
  , issue = issue
  }


-- VIEW

view : Signal.Address a -> Model -> Html
view address model =
  div []
    [ div [ style [ ("font-weight", "bold") ] ] [ text "Assignment" ]
    , div []
      [ span [] [ text "Issue: " ]
      , span [] [ text model.issue.title ]
      ]
    ]
