module Interpreters.ArithmeticAccumulator where

data Command = Add Int | Mul Int | Neg | Print | Ignore
  deriving (Eq, Show)

calcInterpreter :: [String] -> [String]
calcInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case words s of
        ["add", nStr] ->
          case reads nStr of
            [(n,"")] -> Add n
            _        -> Ignore
        ["mul", nStr] ->
          case reads nStr of
            [(n,"")] -> Mul n
            _        -> Ignore
        ["neg"]   -> Neg
        ["print"] -> Print
        _         -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go 0
      where
        go :: Int -> [Command] -> [String]
        go _ [] = []
        go v (cmd:cmds) =
          case cmd of
            Add n  -> go (v+n) cmds
            Mul n  -> go (v*n) cmds
            Neg    -> go (-v) cmds
            Print  -> show v : go v cmds
            Ignore -> go v cmds
