module Interpreters.RobotInterpreter where
data Direction = North | East | South | West
  deriving (Eq, Show)

data Command = LeftTurn | RightTurn | Move | Print | Ignore
  deriving (Eq, Show)

robotInterpreter :: [String] -> [String]
robotInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case s of
        "left"  -> LeftTurn
        "right" -> RightTurn
        "move"  -> Move
        "print" -> Print
        _       -> Ignore

    turnLeft :: Direction -> Direction
    turnLeft North = West
    turnLeft West  = South
    turnLeft South = East
    turnLeft East  = North

    turnRight :: Direction -> Direction
    turnRight North = East
    turnRight East  = South
    turnRight South = West
    turnRight West  = North

    step :: (Int,Int) -> Direction -> (Int,Int)
    step (x,y) North = (x, y+1)
    step (x,y) South = (x, y-1)
    step (x,y) East  = (x+1, y)
    step (x,y) West  = (x-1, y)

    interpreter :: [Command] -> [String]
    interpreter = go (0,0,North)
      where
        go :: (Int,Int,Direction) -> [Command] -> [String]
        go _ [] = []
        go (x,y,d) (cmd:cmds) =
          case cmd of
            LeftTurn  -> go (x,y,turnLeft d) cmds
            RightTurn -> go (x,y,turnRight d) cmds
            Move      -> let (x',y') = step (x,y) d
                         in go (x',y',d) cmds
            Print     -> (show x ++ "," ++ show y ++ "," ++ show d) : go (x,y,d) cmds
            Ignore    -> go (x,y,d) cmds
