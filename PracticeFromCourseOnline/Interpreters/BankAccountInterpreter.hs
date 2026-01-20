module Interpreters.BankAccountInterpreter where

data Command = Deposit Int | Withdraw Int | Balance | Overdrawn | Ignore
  deriving (Eq, Show)

bankInterpreter :: [String] -> [String]
bankInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case words s of
        ["deposit", nStr] ->
          case reads nStr of
            [(n,"")] -> Deposit n
            _        -> Ignore
        ["withdraw", nStr] ->
          case reads nStr of
            [(n,"")] -> Withdraw n
            _        -> Ignore
        ["balance"]   -> Balance
        ["overdrawn"] -> Overdrawn
        _             -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go 0
      where
        go :: Int -> [Command] -> [String]
        go _ [] = []
        go bal (cmd:cmds) =
          case cmd of
            Deposit n  -> go (bal + n) cmds
            Withdraw n -> go (if bal >= n then bal - n else bal) cmds
            Balance    -> show bal : go bal cmds
            Overdrawn  -> (if bal < 0 then "Yes" else "No") : go bal cmds
            Ignore     -> go bal cmds
