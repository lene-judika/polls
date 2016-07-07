module Main (main) where
  import App
  import Command
  import ConnectionManager

  main = runApp (initModel "http://127.0.0.1:8080" :: (Model, Cmd Msg)) update view
