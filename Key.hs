module Key
  ( Key
  , keyFromChar
  , composeKey
  , leftArrowKey
  , rightArrowKey
  , upArrowKey
  , downArrowKey
  , render
  ) where

import Data.Char

-- | 'Key' @s@ will expand to @"<s>"@.
newtype Key = Key String
  deriving (Eq, Ord, Read, Show)

keyFromChar :: Char -> Key
keyFromChar c =
  if isAscii c && isAlphaNum c
  then Key [c]
  else case lookup c keyFromCharData of
         Just s -> Key s
         Nothing -> error $ concat ["keyFromChar: `", [c], "'"]

keyFromCharData :: [(Char, String)]
keyFromCharData =
  -- NOTE: /usr/include/X11/keysymdef.h
  [ let c = '\'' in
    ( c , "apostrophe")
  , ('`', "grave")
  , ('<', "less")
  , ('>', "greater")
  , ('|', "bar")
  , ('(', "parenleft")
  , (')', "parenright")
  , ('[', "bracketleft")
  , (']', "bracketright")
  , ('{', "braceleft")
  , ('}', "braceright")
  , ('+', "plus")
  , ('-', "minus")
  , ('^', "asciicircum")
  , ('.', "period")
  , ('=', "equal")
  , ('~', "asciitilde")
  , ('/', "slash")
  , let c = '\\' in
    ( c , "backslash")
  , ('?', "question")
  , ('!', "exclam")
  , ('_', "underscore")
  , (':', "colon")
  , (';', "semicolon")
  , ('*', "asterisk")
  , ('"', "quotedbl")
  , ('#', "numbersign")
  , ('&', "ampersand")
  , (',', "comma")
  , (' ', "space")
  , let c = '\n' in
    ( c , "Return")
  , ('→', "Right")
  , ('←', "Left")
  , ('↑', "Up")
  , ('↓', "Down") ]

composeKey :: Key
composeKey = Key "Multi_key"

leftArrowKey :: Key
leftArrowKey = Key "Left"

rightArrowKey :: Key
rightArrowKey = Key "Right"

upArrowKey :: Key
upArrowKey = Key "Up"

downArrowKey :: Key
downArrowKey = Key "Down"

render :: Key -> String
render (Key s) = concat [ "<", s, ">" ]

