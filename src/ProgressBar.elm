-- The ProgressBar is a table row (tr) with
-- a "meter" span.
--
-- The meter is determined by "progress" and
-- "max".
--
-- Rules to draw the meter:
--   - The meter width is determined by the absolute percentage
--     of progress / max.
--   - If the value is negative, the meter color changes.
--   - If abs(progress / max) > 1, the meter is full.

module ProgressBar where

import Html exposing (..)
import Html.Attributes exposing (class, colspan, style)

-- VIEW

view : Float -> Html
view value =
  let
    color =
      if value < 0 then
        "red"
      else
        "green"
    absoluteValue = min 100 (abs value)
    styleWidth = (toString absoluteValue) ++ "%"
    meterText = color ++ ", " ++ styleWidth
  in
    tr [ class "meter" ]
      [ td [ colspan 3 ]
        [ span
          [ class "meter__level"
          , style
            [ ("width", styleWidth)
            , ("background-color", color)
            ]
          ]
          []
        ]
      ]
