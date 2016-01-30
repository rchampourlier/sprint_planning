import Issue
import SprintPlanning
import TeamMember

import Effects exposing (Never)
import StartApp
import Task

app =
  StartApp.start
    { init = SprintPlanning.init
    , update = SprintPlanning.update
    , view = SprintPlanning.view
    , inputs = []
    }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
