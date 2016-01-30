module Issues where

import Set exposing (Set)

import Issue

teamMembersNames : List Issue.Model -> Set String
teamMembersNames issues =
  let
    insertMaybeName : Maybe String -> Set String -> Set String
    insertMaybeName maybeName names =
      case maybeName of
        Nothing -> names
        Just name -> Set.insert name names
  in
    case issues of
      head :: tail ->
        teamMembersNames tail
          |> insertMaybeName head.developerName
          |> insertMaybeName head.reviewerName
      _ -> Set.empty
