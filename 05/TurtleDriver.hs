module TurtleDriver where
 
import Geometry
import Generator
import Turtle

genCommand :: Int -> Command
genCommand n
  | r == 0 = TurnLeft
  | r == 1 = TurnRight
  | r == 2 = RaisePen
  | r == 3 = LowerPen
  | otherwise = Move (r - 3)
  where
    r = n `mod` 11
 
genCommands :: Int -> [Command]
genCommands seed = map genCommand (generate seed)

turtleHistory :: [Command] -> [Turtle]
turtleHistory = scanl action startTurtle

further :: [Command] -> Int
further cmds =
    length $
    takeWhile (< 400) $
    dropWhile (< 200) $
    map normPoint $ map location $
    turtleHistory cmds