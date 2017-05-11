module Scanner
  ( scan
  ) where

import           Control.Applicative ((<$>))
import           Types               (Token (..))

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just []

-- ein Zeilenumbruch
scan ('\n':xs)    = (TokenNewline : ) <$> scan xs

-- eine Anzahl Leerzeichen
scan str@(' ':_) = let (blanks, rest) = span (==' ') str
    in ( TokenBlanks (length blanks) : ) <$> scan rest

-- Hashes die eine Überschrift markieren oder Text
scan str@('#':_) =
    let (hashes, rest) = span (=='#') str
        level = length hashes
    in (if level <= 6
           then ( TokenH level : )
           else ( TokenText hashes : ))
        <$> scan rest

-- Text ohne die vorher erkannten Zeichen
scan str          =
    let (text, rest) = span (`notElem` "# \n") str
    in (TokenText text : ) <$> scan rest
