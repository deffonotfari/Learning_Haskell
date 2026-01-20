module Interpreters.KeyValueStoreInterpreter where

import Data.List (intercalate)

data Command = Set String Int | Get String | Delete String | Print | Ignore
  deriving (Eq, Show)

storeInterpreter :: [String] -> [String]
storeInterpreter = interpreter . map parse
  where
    parse :: String -> Command
    parse s =
      case words s of
        ["set", k, nStr] ->
          case reads nStr of
            [(n,"")] -> Set k n
            _        -> Ignore
        ["get", k]    -> Get k
        ["delete", k] -> Delete k
        ["print"]     -> Print
        _             -> Ignore

    setKV :: String -> Int -> [(String,Int)] -> [(String,Int)]
    setKV k v [] = [(k,v)]
    setKV k v ((k',v'):xs)
      | k == k'   = (k,v) : xs
      | otherwise = (k',v') : setKV k v xs

    deleteKV :: String -> [(String,Int)] -> [(String,Int)]
    deleteKV _ [] = []
    deleteKV k ((k',v'):xs)
      | k == k'   = xs
      | otherwise = (k',v') : deleteKV k xs

    getKV :: String -> [(String,Int)] -> Maybe Int
    getKV _ [] = Nothing
    getKV k ((k',v'):xs)
      | k == k'   = Just v'
      | otherwise = getKV k xs

    showStore :: [(String,Int)] -> String
    showStore kvs = intercalate "," [k ++ "=" ++ show v | (k,v) <- kvs]

    interpreter :: [Command] -> [String]
    interpreter = go []
      where
        go :: [(String,Int)] -> [Command] -> [String]
        go _ [] = []
        go st (cmd:cmds) =
          case cmd of
            Set k n  -> go (setKV k n st) cmds
            Delete k -> go (deleteKV k st) cmds
            Get k    ->
              case getKV k st of
                Nothing -> "NotFound" : go st cmds
                Just n  -> show n : go st cmds
            Print    -> showStore st : go st cmds
            Ignore   -> go st cmds
