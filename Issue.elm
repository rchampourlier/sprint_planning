module Issue (Model, init, view) where

import Html exposing (..)


-- MODEL

type alias Model =
  { title : String
  , estimate : Float
  }

init : String -> Float -> Model
init title estimate =
  { title = title
  , estimate = estimate
  }


-- VIEW

view : Model -> Html
view model =
  div []
    [ div [] [ text model.title ]
    , div []
      [ span [] [ text "Estimate: " ]
      , span [] [ text <| toString model.estimate ]
      ]
    ]
