module Interpreters.SwapInterpreter where

data Command = IncA | DecA | IncB | DecB | Swap | Print | Ignore
  deriving (Eq, Show)

swapInterpreter :: [String] -> [String]
swapInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case s of
        "incA"  -> IncA
        "decA"  -> DecA
        "incB"  -> IncB
        "decB"  -> DecB
        "swap"  -> Swap
        "print" -> Print
        _       -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go (0,0)
      where
        go :: (Int,Int) -> [Command] -> [String]
        go _ [] = []
        go (a,b) (cmd:cmds) =
          case cmd of
            IncA   -> go (a+1,b) cmds
            DecA   -> go (a-1,b) cmds
            IncB   -> go (a,b+1) cmds
            DecB   -> go (a,b-1) cmds
            Swap   -> go (b,a) cmds
            Print  -> (show a ++ "," ++ show b) : go (a,b) cmds
            Ignore -> go (a,b) cmds
