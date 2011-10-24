import Data.List

import Key
import Source
import qualified Trie as T

main :: IO ()
main = putStr . unlines . map Main.render $ T.toList source

render :: ([Key], String) -> String
render (ks, a) = intercalate " " $
  map Key.render (composeKey : ks) ++ [ concat [ ": \"", a, "\"" ] ]

