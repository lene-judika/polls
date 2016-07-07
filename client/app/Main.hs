module Main (main) where
  import RunApp
  import Command
  import ConnectionManager
  import Component

  main = runApp (initModel "http://127.0.0.1:8080" :: (Model, Cmd Msg)) update view
