module XML where

import Data.Char

-- XML element
data Element = Element Name [Attribute] [Content]
type Name = String
type Attribute = (Name, String)
data Content = Text String | Child Element

-- string representation of an XML element
printElement :: Element -> String
printElement (Element n attrs []) =
    "<" ++ unwords (n : map printAttr attrs) ++ "/>"
printElement (Element n attrs body) =
    "<" ++ unwords (n : map printAttr attrs) ++ ">" ++
    concat (map printContent body) ++
    "</" ++ n ++ ">"

-- string representation of an attribute-value pair
printAttr :: Attribute -> String
printAttr (n, val) = n ++ "=\"" ++ encodeString val ++ "\""

-- string representation of the content of an XML element
printContent :: Content -> String
printContent (Text s) = encodeString s
printContent (Child e) = printElement e

-- encoding of a string value
encodeString :: String -> String
encodeString = concat . map encodeChar

-- encoding of a character, with character escapes
encodeChar :: Char -> String
encodeChar '<' = "&lt;"
encodeChar '>' = "&gt;"
encodeChar '&' = "&amp;"
encodeChar '"' = "&quot;"
encodeChar c
  | isAscii c = [c]
  | otherwise = "&#" ++ show (ord c) ++ ";"