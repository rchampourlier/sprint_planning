import Assignment
import Issue
import SprintPlanning
import TeamMember

import StartApp.Simple exposing (start)

main =
  let
    issue1 = Issue.init "Issue 1" 40
    issue2 = Issue.init "Issue 2" 20

    romain = TeamMember.init "Romain" 20 []
    david = TeamMember.init "David" 120 []
    teamMembers = [(0, romain), (1, david)]

    unassignedIssues = [ issue1, issue2 ]
    assignments = []

  in
    {- Beware, with the StartApp.Simple, you can't change
       the signature of the view function which must have
       SignalAddress a -> Model -> Html
    -}
    start
      { model = SprintPlanning.init unassignedIssues assignments teamMembers
      , update = SprintPlanning.update
      , view = SprintPlanning.view
      }
