import Issue
import SprintPlanning
import TeamMember

import Effects exposing (Never)
import StartApp
import Task

app =
  let
    issues = [
      Issue.init "PJ-0001" "Some issue from the Product Owner" 10,
      Issue.init "PJ-0002" "Something that the customers would love" 20,
      Issue.init "PJ-0003" "Some technical task preventing everything to crash" 5
    ]

    teamMembers = [
      TeamMember.init "Paul" 10,
      TeamMember.init "Jean" 20,
      TeamMember.init "Pierre" 15
    ]

  in
    {- Beware, with the StartApp.Simple, you can't change
       the signature of the view function which must have
       SignalAddress a -> Model -> Html
    -}
    StartApp.start
      { init = SprintPlanning.init issues teamMembers
      , update = SprintPlanning.update
      , view = SprintPlanning.view
      , inputs = []
      }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
