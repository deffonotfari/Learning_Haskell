module Practice2 where

-- Commands:
-- "inc", "dec", "print"
-- Start at 0

miniInterpreter :: [String] -> [String]
miniInterpreter = go 0
    where
        go :: Int -> [String] -> [String]
        go _ [] = []
        go n (c:cs)
            | c == "inc"    = go (n+1) cs
            | c == "dec"    = go (n-1) cs
            | c == "print"  = show n : go n cs
            | otherwise     = go n cs


-- commands:
counterInterpreter :: [String] -> [String]
counterInterpreter = go 0
  where
    go _ [] = []
    go n (c:cs)
      | c == "inc"   = go (n+1) cs
      | c == "dec"   = go (n-1) cs
      | c == "print" = show n : go n cs
      | otherwise    = go n cs

--define a function that maintains a boolean state, initially false
boolInterpreter :: [String] -> [String]
boolInterpreter = go False
    where
        go _ [] = []
        go n (cmd:cmds)
            | cmd == "on"     = go True cmds
            | cmd == "dec"    = go False cmds
            | cmd == "print"  = show n : go n cmds
            | otherwise    = go n cmds

xyInterpreter :: [String] -> [String]
xyInterpreter = go 0 0
  where
    go _ _ [] = []
    go x y (cmd:cmds) =
      case cmd of
        "incX"      -> go (x+1) y cmds
        "decX"      -> go (x-1) y cmds
        "incY"      -> go x (y+1) cmds
        "decY"      -> go x (y-1) cmds
        "print"     -> (show  x ++","++ show y) : go x y cmds
        _           -> go x y cmds


sumInterpreter :: [String] -> [String]
sumInterpreter = go 0
  where
    go _ [] = []
    go n (c:cs)
      | take 3 c == "add" =
          let k = read (drop 4 c)
          in go (n+k) cs
      | c == "print" = show n : go n cs
      | otherwise    = go n cs

-- toggle interpreter
data Command = Toggle | On | Off | Print | Ignore
  deriving (Eq, Show)

toggleInterpreter :: [String] -> [String]
toggleInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case s of
        "toggle" -> Toggle
        "on"     -> On
        "off"    -> Off
        "print"  -> Print
        _        -> Ignore

    interpreter :: [Command] -> [String]
    interpreter = go False
      where
        go :: Bool -> [Command] -> [String]
        go _ [] = []
        go st (cmd:cmds) =
          case cmd of
            Toggle -> go (not st) cmds
            On     -> go True cmds
            Off    -> go False cmds
            Print  -> show st : go st cmds
            Ignore -> go st cmds
