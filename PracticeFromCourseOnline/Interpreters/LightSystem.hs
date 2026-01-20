module Interpreters.LightSystem where

data Light = Off | Dim | Bright
  deriving (Eq, Show)

data Command = Up | Down | Print | Ignore
  deriving (Eq, Show)

lightInterpreter :: [String] -> [String]
lightInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case s of
        "up"    -> Up
        "down"  -> Down
        "print" -> Print
        _       -> Ignore

    up :: Light -> Light
    up Off    = Dim
    up Dim    = Bright
    up Bright = Bright

    down :: Light -> Light
    down Bright = Dim
    down Dim    = Off
    down Off    = Off

    interpreter :: [Command] -> [String]
    interpreter = go Off
      where
        go :: Light -> [Command] -> [String]
        go _ [] = []
        go st (cmd:cmds) =
          case cmd of
            Up     -> go (up st) cmds
            Down   -> go (down st) cmds
            Print  -> show st : go st cmds
            Ignore -> go st cmds
