data Command
  = Let Int
  | Add Int
  | IfZero Command
  | Print
  | Ignore
  deriving (Eq, Show)

miniLang :: [String] -> [String]
miniLang = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case words s of
        ["let", nStr] ->
          case reads nStr of
            [(n,"")] -> Let n
            _        -> Ignore
        ["add", nStr] ->
          case reads nStr of
            [(n,"")] -> Add n
            _        -> Ignore
        ["print"] -> Print
        ["ifzero", one] ->
          -- "one" is a *single command token* (e.g. "print" or "neg" in variants).
          -- We parse it as a command string itself.
          IfZero (parse one)
        _ -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go 0
      where
        go :: Int -> [Command] -> [String]
        go _ [] = []
        go v (cmd:cmds) =
          case cmd of
            Let n      -> go n cmds
            Add n      -> go (v+n) cmds
            Print      -> show v : go v cmds
            IfZero c   ->
              if v == 0
                then goExec v c cmds   -- execute immediately, then continue
                else go v cmds
            Ignore     -> go v cmds

        -- Execute exactly one command "c" immediately, producing outputs and a new state,
        -- then continue with remaining cmds.
        goExec :: Int -> Command -> [Command] -> [String]
        goExec v c rest =
          case c of
            Let n    -> go n rest
            Add n    -> go (v+n) rest
            Print    -> show v : go v rest
            IfZero c2 ->
              if v == 0 then goExec v c2 rest else go v rest
            Ignore   -> go v rest
