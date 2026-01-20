module Interpreters.BoundedCounter where
data Command = Inc | Dec | Print | Ignore
  deriving (Eq, Show)

boundedCounter :: Int -> Int -> [String] -> [String]
boundedCounter lo hi = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case s of
        "inc"   -> Inc
        "dec"   -> Dec
        "print" -> Print
        _       -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go lo
      where
        go :: Int -> [Command] -> [String]
        go _ [] = []
        go n (cmd:cmds) =
          case cmd of
            Inc    -> go (if n >= hi then n else n+1) cmds
            Dec    -> go (if n <= lo then n else n-1) cmds
            Print  -> show n : go n cmds
            Ignore -> go n cmds
