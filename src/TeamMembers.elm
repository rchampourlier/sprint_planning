module TeamMembers where

import List
import Set exposing (Set)

import TeamMember

fromNames : Set String -> List TeamMember.Model
fromNames names =
  Set.toList names
    |> List.map (\n -> TeamMember.init n 0)

toNames : List TeamMember.Model -> List String
toNames teamMembers =
  List.map (\tm -> tm.name) teamMembers
