module TeamMember where

import Html exposing (..)


-- MODEL

type alias Model =
  { capacity : Float
  , name : String
  }

init : String -> Float -> Model
init name capacity =
  { capacity = capacity
  , name = name
  }


-- VIEW

view : Model -> Float -> Float -> Html
view model developerEstimate reviewerEstimate =
  div []
    [ h4 [] [ text model.name ]
    , div []
      [ span [] [ text "Initial capacity: " ]
      , span [] [ text <| toString model.capacity ]
      ]
    , div []
      [ span [] [ text "Remaining: " ]
      , span [] [ text <| toString (model.capacity - developerEstimate) ]
      ]
    , div []
      [ span [] [ text <| "Review estimate: " ++ (toString reviewerEstimate) ] ]
    ]
