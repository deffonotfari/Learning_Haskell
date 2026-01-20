module TripleVariableInterpreter where

data Command = IncX | IncY | IncZ | DecX | DecY | DecZ | Print | Ignore
    deriving (Show, Eq)

tripleVariableInterpreter :: [String] -> [String]
tripleVariableInterpreter = interpreter . map parse
    where
        parse :: String -> Command
        parse s =
            case s of 
                "incX"      -> IncX
                "incY"      -> IncY
                "incZ"      -> IncZ
                "decX"      -> DecX
                "decY"      -> DecY
                "decZ"      -> DecZ
                "print"     -> Print
                _           -> Ignore
        
        interpreter :: [Command] -> [String]
        interpreter = go (0, 0, 0)
            where
                go :: (Int, Int, Int) -> [Command] -> [String]
                go _ [] = []
                go (x, y, z) (cmd: cmds) =
                    case cmd of
                        IncX    -> go (x+1, y, z) cmds
                        IncY    -> go (x, y+1, z) cmds
                        IncZ    -> go (x, y, z+1) cmds
                        DecX    -> go (x-1, y, z) cmds
                        DecY    -> go (x, y-1, z) cmds
                        DecZ    -> go (x, y, z-1) cmds
                        Print   -> (show x ++ "," ++ show y ++ ","++show z) : go (x,y,z) cmds
                        Ignore  -> go (x,y,z) cmds