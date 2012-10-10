module Source ( source ) where

import Control.Arrow
import Data.Char

import Key
import Trie (Trie)
import qualified Trie as Trie

{- DOC:

The shortcuts are organized as follow:

\-* Multi_Key
  +-* ' alpha -> acute
  +-* ` alpha -> grave
  +-* ^ alpha -> circumflex
  +-* " alpha -> diaeresis
  +-* , alpha -> cedilla
  +-* ?
  | +-* ~ alpha -> tilde
  | \-* o alpha -> ring
  +-* \
  | +-* alpha* ; -> latex
  | +-* ^ char -> superscript
  | +-* _ char -> subscript
  | +-* * char -> bold
  | +-* / char -> italic
  | +-* /* char -> bold italic
  | +-* | char -> struck
  | +-* ! char -> fraktur
  | +-* !* char -> fraktur bold
  | +-* ~ char -> sans-serif
  | +-* ~* char -> sans-serif bold
  | +-* ~/ char -> sans-serif italic
  | +-* ~/* char -> sans-serif italic bold
  | +-* [ char -> monospace
  | +-* g~* char -> greek sans-serif bold
  | +-* g~/* char -> greek sans-serif italic bold
  | +-* { char -> script
  | \-* {* char -> script bold
  +-* g
  | +-* char -> greek
  | +-* * char -> greek bold
  | +-* / char -> greek italic
  | \-* /* char -> greek italic bold
  \ ... -> symbol

Starting characters for symbols are:
TODO: generate them

Tricks to remember symbols shortcuts:
* r rotation/symmetry
* c center
* b big
* s small
* p pair
* → ↑ ← ↓ arrows

=Arrows=

Format: {prefix} arrow

Exemples in prefix order:
← for ←
p← for ↔
r← for ↻
b← for ⟵
=← for ⇐
|← for ↤
~← for ↜
s~← for ⇜
.← for ⇠
<← for ↞
>← for ↢
/← for ↼
\← for ↽
||← for ⇤
-|→ for ⇸

-}

type Assoc = (String, String)
type Assocs = [Assoc]

  -- -- quotes
  -- ,("\"","“”")
  -- ,("r`","′")

source :: Trie Key String
source = trieFromAssocs $ concat
  [ letters
  , symbols ]

letters :: Assocs
letters = concat
  [ accents
  , superscripts
  , subscripts
  , math_alphanum ]

symbols :: Assocs
symbols = concat
  [ parens
  , quantifiers
  , operators
  , arrows
  , relations
  , equals
  , misc
  , currency
  , punctuation
  , turnstyles ]

accents :: Assocs
accents =
  [("oe", "œ"), ("OE", "Œ")] ++ concat
  [ f 'a' 'á' 'à' 'â' 'ä' 'ã' 'å' ' '
  , f 'c' 'ć' ' ' ' ' ' ' ' ' ' ' 'ç'
  , f 'e' 'é' 'è' 'ê' 'ë' ' ' ' ' ' '
  , f 'i' 'í' 'ì' 'î' 'ï' ' ' ' ' ' '
  , f 'n' 'ń' ' ' ' ' ' ' 'ñ' ' ' ' '
  , f 'o' 'ó' 'ò' 'ô' 'ö' 'õ' ' ' ' '
  , f 'u' 'ú' 'ù' 'û' 'ü' ' ' ' ' ' '
  , f 'y' 'ý' ' ' ' ' 'ÿ' ' ' ' ' ' '
  , f 'A' 'Á' 'À' 'Â' 'Ä' 'Ã' 'Å' ' '
  , f 'C' 'Ć' ' ' ' ' ' ' ' ' ' ' 'Ç'
  , f 'E' 'É' 'È' 'Ê' 'Ë' ' ' ' ' ' '
  , f 'I' 'Í' 'Ì' 'Î' 'Ï' ' ' ' ' ' '
  , f 'N' 'Ń' ' ' ' ' ' ' 'Ñ' ' ' ' '
  , f 'O' 'Ó' 'Ò' 'Ô' 'Ö' 'Õ' ' ' ' '
  , f 'U' 'Ú' 'Ù' 'Û' 'Ü' ' ' ' ' ' '
  , f 'Y' 'Ý' ' ' ' ' ' ' ' ' ' ' ' ' ]
  where
    f k acute grave circumflex diaeresis tilde ring cedilla =
      -- filter ((/= " ") . snd)
      [ let c = '\'' in
        b  c  acute
      , b '`' grave
      , b '^' circumflex
      , b '"' diaeresis
      , d '~' tilde
      , d 'o' ring
      , b ',' cedilla ]
      where
        b p l = ([p, k], [l])
        d p l = (['?', p, k], [l])

parens :: Assocs
parens = concat
  [ l "<;" "⟨" ">;" "⟩" "langle" "rangle"
  , s "b<<" "⟪" "b>>" "⟫"
  , s "<<" "«" ">>" "»"
  , s "{|" "⦃" "|}" "⦄"
  , l "[|" "⟦" "|]" "⟧" "llbracket" "rrbracket"
  , l "|_" "⌊" "_|" "⌋" "lfloor" "rfloor"
  , l "r|_" "⌈" "r_|" "⌉" "lceil" "rceil" ]
  where
    s ls lc rs rc = [(ls, lc), (rs, rc), ('p':ls, lc ++ rc)]
    l ls lc rs rc ll rl =
      (latex ll, lc) : (latex rl, rc) : s ls lc rs rc

quantifiers :: Assocs
quantifiers = assocsFromList
  [ f "rA" "forall" "∀"
  , f "rE" "exists" "∃"
  , f "/rE" "nexists" "∄" ]
  where f short long code = ([short, latex long], code)

operators :: Assocs
operators = assocsFromList
  [ l "U" "cup" "∪"
  , l "rU" "cap" "∩"
  , l "+U" "uplus" "⊎"
  , l "[U" "sqcup" "⊔"
  , l "r[U" "sqcap" "⊓"
  , l "/\\" "wedge" "∧"
  , l "r/\\" "vee" "∨"
  , l "co" "circ" "∘"
  , l "c.;" "cdot" "·"
  , l ".." "ldots" "…"
  , l "r.." "vdots" "⋮"
  , l "c.." "cdots" "⋯"
  , l "cx" "times" "×"
  , l "-," "neg" "¬"
  , s "o+" "⊕"
  , s "o-" "⊖"
  , s "ox" "⊗"
  , s "o/" "⊘"
  , s "o*" "⊛"
  , s "o=" "⊜"
  , s "o." "⊙"
  , s "r&" "⅋" ]
  where
    l short long code = ([short, latex long], code)
    s short code = ([short], code)

arrows :: Assocs
arrows =
  concat
  [ w ("", "") ("", "arrow")
      [ ("←", "left")
      , ("→", "right")
      , ("↔", "leftright")
      , ("↑", "up")
      , ("↓", "down")
      , ("↕", "updown") ]
  , w ("=", "") ("", "arrow")
      [ ("⇐", "Left")
      , ("⇒", "Right")
      , ("⇔", "Leftright")
      , ("⇑", "Up")
      , ("⇓", "Down")
      , ("⇕", "Updown") ]
  , w ("b", "long") ("", "arrow")
      [ ("⟵", "left")
      , ("⟶", "right")
      , ("⟷", "leftright") ]
  , w ("b=", "Long") ("", "arrow")
      [ ("⟸", "left")
      , ("⟹", "right")
      , ("⟺", "leftright") ]
  , w ("|", "maps") ("", "")
      [ ("↤", "from")
      , ("↦", "to") ]
  , w ("b|", "longmaps") ("", "")
      [ ("⟻", "from")
      , ("⟼", "to") ]
  , w ("~", "leads") ("", "")
      [ ("↜", "from")
      , ("↝", "to") ] ]
  ++ assocsFromList
  [ s "s<|" "◃"
  , s "s|>" "▹"
  , l "<|" "◁" "triangleleft"
  , l "|>" "▷" "triangleright"
  , s "<)" "◅"
  , s "(>" "▻"
  , s "s~←" "⇜"
  , s "s~→" "⇝"
  , s ">←" "↢"
  , s ">→" "↣"
  , s "<←" "↞"
  , s "<↑" "↟"
  , s "<→" "↠"
  , s "<↓" "↡"
  , s "||→" "⇥"
  , s "||←" "⇤"
  , s ".←" "⇠"
  , s ".↑" "⇡"
  , s ".→" "⇢"
  , s ".↓" "⇣"
  , s "r→" "↺"
  , s "r←" "↻"
  , s "/←" "↼"
  , s "/→" "⇀"
  , s "\\←" "↽"
  , s "\\→" "⇁"
  , s "-|→" "⇸" ]
  where
    s short code = ([short], code)
    l short code long = ([short, latex long], code)
    w :: (String, String) -> (String, String)
      -> [(String, String)] -> Assocs
    w (preshort, prelong) (sufshort, suflong) codenames = do
      ((pre, dir), (code, name)) <- zip schema codenames
      [(short pre dir, code), (long name, code)]
      where
        short pre dir = concat [pre, preshort, [dir], sufshort]
        long name = latex $ concat [prelong, name, suflong]
        schema = zip (cycle ["", "", "p"]) "←→←↑↓↑"

relations :: Assocs
relations =
  [ ("_C","⊆")
  , ("/_C","⊈")
  , ("C","⊂")
  , ("/C","⊄")
  , ("-C","∈")
  , ("/-C","∉")
  , ("_/C","⊊")
  , ("r_C","⊇")
  , ("rC","⊃")
  , ("r-C","∋")
  , ("r/-C","∌")
  , ("r_/C","⊋")
  , ("_>","≥")
  , ("_<","≤")
  , ("/_>","≱")
  , ("/_<","≰")
  , ("[C","⊏")
  , ("r[C","⊐")
  , ("_[C","⊑")
  , ("r_[C","⊒")
  , ("/_[C","⋢")
  , ("r/_[C","⋣")
  , ("_/[C","⋤")
  , ("r_/[C","⋥") ]

equals :: Assocs
equals =
  [ ("=^", "≙")
  , ("=v", "≚")
  , ("=*", "≛")
  , ("=(", "≘")
  , ("=<|", "≜")
  , ("=def","≝")
  , ("=?","≟")
  , ("=o","≗")
  , ("=m","≞")
  , ("_=","≡")
  , ("~~","≈")
  , ("_~","≃")
  , ("=~","≅")
  , ("b~","∼")
  , ("/=","≠")
  , ("/_=","≢")
  , (":=","≔")
  , ("=:","≕") ]

misc :: Assocs
misc =
  [ ("Bot", "⊥")
  , ("Top", "⊤")
  , ("::", "∷")
  , ("/O", "∅")
  , ("/0", "∅")
  , ("r8", "∞")
  , (";diamond", "◇")
  , ("--","−")
  , (";star", "★")
  -- , ("star", "⋆")
  , ("+-", "±")
  , ("/gl", "ƛ")
  , ("#b", "♭")
  , ("#f", "♮")
  , ("##", "♯")
  , (";cut", "✂")
  , (";pen", "✎")
  , (";tick", "✓")
  , ("<3", "♥")
  , ("oo", "°")
  , (";box", "☐")
  , ("[ ]", "☐")
  , ("[v]", "☑") ]

punctuation :: Assocs
punctuation =
  [ ("!!", "‼")
  , ("??", "⁇")
  , ("?!", "⁈")
  , ("!?", "⁉")
  , ("r?", "¿")
  , ("r!", "¡")
  , ("  ", " ") ]

currency :: Assocs
currency =
  [ ("E=","€")
  , ("L-","£") ]

turnstyles :: Assocs
turnstyles =
  [ ("|-", "⊢")
  , ("/|-", "⊬")
  , ("r|-", "⊣")
  , ("|=", "⊨")
  , ("/|=", "⊭")
  , ("||-", "⊩") ]

zipscripts :: Char -> String -> String -> Assocs
zipscripts c ascii unicode =
  zip (fmap ((['\\', c] ++) . (: [])) ascii) (fmap (: []) unicode)

subscripts :: Assocs
subscripts = zipscripts '_' "0123456789+-=()aeioruvx"
                            "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"

superscripts :: Assocs
superscripts = zipscripts '^' -- NOTE: qCFQSVXYZ are missing
  "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
  "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"

math_alphanum :: Assocs
math_alphanum = concat
  [ a "\\*" math_bold
  , n "\\*" math_bold_digit
  , a "\\/" math_italic
  , a "\\/*" math_bold_italic
  , a "\\{" math_script
  , a "\\{*" math_bold_script
  , a "\\|" math_struck
  , n "\\|" math_struck_digit
  , a "\\!" math_fraktur
  , a "\\!*" math_bold_fraktur
  , a "\\~" math_sansserif
  , n "\\~" math_sansserif_digit
  , a "\\~*" math_sansserif_bold
  , n "\\~*" math_sansserif_bold_digit
  , a "\\~/" math_sansserif_italic
  , a "\\~/*" math_sansserif_bold_italic
  , a "\\[" math_mono
  , n "\\[" math_mono_digit
  , gl "g" math_greek
  , gs "g*" math_bold_greek
  , gs "g/" math_italic_greek
  , gs "g/*" math_bold_italic_greek
  , gs "\\g~*" math_sansserif_bold_greek
  , gs "\\g~/*" math_sansserif_bold_italic_greek ]
  where
    f chars prefix alphas = do
      (char, alpha) <- zip chars alphas
      return (prefix ++ [char], [alpha])
    a = f $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ++ "abcdefghijklmnopqrstuvwxyz"
    n = f $ "0123456789"
    g prefix alphas b = do
      ((mshort, long), alpha) <- zip greek alphas
      map (\x -> (x, [alpha]))
        $ (if b then [latex long] else [])
          ++ case mshort of
               Nothing -> []
               Just short -> [prefix ++ [short]]
    gl p as = g p as True
    gs p as = g p as False
    capitalize (Nothing, c:cs) = (Nothing, toUpper c : cs)
    capitalize (Just s, c:cs) = (Just (toUpper s), toUpper c : cs)
    capitalize (_, "") = error "Source.math_alphanum.capitalize"
    greek = (\x -> map capitalize x ++ x)
      [ s 'a' "alpha"
      , s 'b' "beta"
      , s 'g' "gamma"
      , s 'd' "delta"
      , s 'e' "epsilon"
      , s 'z' "zeta"
      , s 'n' "eta"
      , l     "theta"
      , s 'i' "iota"
      , s 'k' "kappa"
      , s 'l' "lambda"
      , s 'm' "mu"
      , s 'v' "nu"
      , s 'x' "xi"
      , s 'o' "omicron"
      , s 'p' "pi"
      , s 'r' "rho"
      , l     "varsigma"
      , s 's' "sigma"
      , s 't' "tau"
      , l     "upsilon"
      , s 'f' "phi"
      , s 'c' "chi"
      , s 'y' "psi"
      , s 'w' "omega" ]
      where
        s c cs = (Just c, cs)
        l   cs = (Nothing, cs)

math_greek :: String
math_greek = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡ ΣΤΥΦΧΨΩ"
             ++ "αβγδεζηθικλμνξοπρςστυφχψω"

-- MATHEMATICAL BOLD CAPITAL A TO SMALL Z (U+1D400 TO U+1D433)
math_bold :: String
math_bold = "𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙"
            ++ "𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳"

-- MATHEMATICAL ITALIC CAPITAL A TO SMALL Z (U+1D434 TO U+1D467)
math_italic :: String
math_italic = "𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍"
              ++ "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"

-- MATHEMATICAL BOLD ITALIC CAPITAL A TO SMALL Z (U+1D468 TO U+1D49B)
math_bold_italic :: String
math_bold_italic = "𝑨𝑩𝑪𝑫𝑬𝑭𝑮𝑯𝑰𝑱𝑲𝑳𝑴𝑵𝑶𝑷𝑸𝑹𝑺𝑻𝑼𝑽𝑾𝑿𝒀𝒁"
                   ++ "𝒂𝒃𝒄𝒅𝒆𝒇𝒈𝒉𝒊𝒋𝒌𝒍𝒎𝒏𝒐𝒑𝒒𝒓𝒔𝒕𝒖𝒗𝒘𝒙𝒚𝒛"

-- MATHEMATICAL SCRIPT CAPITAL A TO SMALL Z (U+1D49C TO U+1D4CF)
math_script :: String
math_script = "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"
              ++ "𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏"

-- MATHEMATICAL BOLD SCRIPT CAPITAL A TO SMALL Z (U+1D4D0 TO U+1D502)
math_bold_script :: String
math_bold_script = "𝓐𝓑𝓒𝓓𝓔𝓕𝓖𝓗𝓘𝓙𝓚𝓛𝓜𝓝𝓞𝓟𝓠𝓡𝓢𝓣𝓤𝓥𝓦𝓧𝓨𝓩"
                   ++ "𝓪𝓫𝓬𝓭𝓮𝓯𝓰𝓱𝓲𝓳𝓴𝓵𝓶𝓷𝓸𝓹𝓺𝓻𝓼𝓽𝓾𝓿𝔀𝔁𝔂𝔃"

-- MATHEMATICAL FRAKTUR CAPITAL A TO SMALL Z (U+1D504 TO U+1D537)
math_fraktur :: String
math_fraktur = "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ"
               ++ "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷"

-- MATHEMATICAL DOUBLE-STRUCK CAPITAL A TO SMALL Z (U+1D538 TO U+1D56B)
math_struck :: String
math_struck = "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ"
              ++ "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫"

-- MATHEMATICAL BOLD FRAKTUR CAPITAL A TO SMALL Z (U+1D56C TO U+1D59F)
math_bold_fraktur :: String
math_bold_fraktur = "𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅"
                    ++ "𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟"

-- MATHEMATICAL SANS-SERIF CAPITAL A TO SMALL Z (U+1D5A0 TO U+1D5D3)
math_sansserif :: String
math_sansserif = "𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹"
                 ++ "𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓"

-- MATHEMATICAL SANS-SERIF BOLD CAPITAL A TO SMALL Z (U+1D5D4 TO U+1D607)
math_sansserif_bold :: String
math_sansserif_bold = "𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭"
                      ++ "𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇"

-- MATHEMATICAL SANS-SERIF ITALIC CAPITAL A TO SMALL Z (U+1D608 TO U+1D63B)
math_sansserif_italic :: String
math_sansserif_italic = "𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡"
                        ++ "𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻"

-- MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A TO SMALL Z (U+1D63C TO U+1D66F)
math_sansserif_bold_italic :: String
math_sansserif_bold_italic = "𝘼𝘽𝘾𝘿𝙀𝙁𝙂𝙃𝙄𝙅𝙆𝙇𝙈𝙉𝙊𝙋𝙌𝙍𝙎𝙏𝙐𝙑𝙒𝙓𝙔𝙕"
                             ++ "𝙖𝙗𝙘𝙙𝙚𝙛𝙜𝙝𝙞𝙟𝙠𝙡𝙢𝙣𝙤𝙥𝙦𝙧𝙨𝙩𝙪𝙫𝙬𝙭𝙮𝙯"

-- MATHEMATICAL MONOSPACE CAPITAL A TO SMALL Z (U+1D670 TO U+1D6A3)
math_mono :: String
math_mono = "𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉"
            ++ "𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣"

-- MATHEMATICAL BOLD CAPITAL ALPHA TO SMALL OMEGA (U+1D6A8 TO U+1D6DA)
math_bold_greek :: String
math_bold_greek = "𝚨𝚩𝚪𝚫𝚬𝚭𝚮𝚯𝚰𝚱𝚲𝚳𝚴𝚵𝚶𝚷𝚸𝚹𝚺𝚻𝚼𝚽𝚾𝚿𝛀"
                  ++ "𝛂𝛃𝛄𝛅𝛆𝛇𝛈𝛉𝛊𝛋𝛌𝛍𝛎𝛏𝛐𝛑𝛒𝛓𝛔𝛕𝛖𝛗𝛘𝛙𝛚"

-- U+1D6C1	𝛁	f0 9d 9b 81	MATHEMATICAL BOLD NABLA
-- U+1D6DB	𝛛	f0 9d 9b 9b	MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
-- U+1D6DC	𝛜	f0 9d 9b 9c	MATHEMATICAL BOLD EPSILON SYMBOL
-- U+1D6DD	𝛝	f0 9d 9b 9d	MATHEMATICAL BOLD THETA SYMBOL
-- U+1D6DE	𝛞	f0 9d 9b 9e	MATHEMATICAL BOLD KAPPA SYMBOL
-- U+1D6DF	𝛟	f0 9d 9b 9f	MATHEMATICAL BOLD PHI SYMBOL
-- U+1D6E0	𝛠	f0 9d 9b a0	MATHEMATICAL BOLD RHO SYMBOL
-- U+1D6E1	𝛡	f0 9d 9b a1	MATHEMATICAL BOLD PI SYMBOL

-- MATHEMATICAL ITALIC CAPITAL ALPHA TO SMALL OMEGA (U+1D6E2 TO U+1D714)
math_italic_greek :: String
math_italic_greek = "𝛢𝛣𝛤𝛥𝛦𝛧𝛨𝛩𝛪𝛫𝛬𝛭𝛮𝛯𝛰𝛱𝛲𝛳𝛴𝛵𝛶𝛷𝛸𝛹𝛺"
                    ++ "𝛼𝛽𝛾𝛿𝜀𝜁𝜂𝜃𝜄𝜅𝜆𝜇𝜈𝜉𝜊𝜋𝜌𝜍𝜎𝜏𝜐𝜑𝜒𝜓𝜔"

-- U+1D6FB	𝛻	f0 9d 9b bb	MATHEMATICAL ITALIC NABLA
-- U+1D715	𝜕	f0 9d 9c 95	MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
-- U+1D716	𝜖	f0 9d 9c 96	MATHEMATICAL ITALIC EPSILON SYMBOL
-- U+1D717	𝜗	f0 9d 9c 97	MATHEMATICAL ITALIC THETA SYMBOL
-- U+1D718	𝜘	f0 9d 9c 98	MATHEMATICAL ITALIC KAPPA SYMBOL
-- U+1D719	𝜙	f0 9d 9c 99	MATHEMATICAL ITALIC PHI SYMBOL
-- U+1D71A	𝜚	f0 9d 9c 9a	MATHEMATICAL ITALIC RHO SYMBOL
-- U+1D71B	𝜛	f0 9d 9c 9b	MATHEMATICAL ITALIC PI SYMBOL

-- MATHEMATICAL BOLD ITALIC CAPITAL ALPHA TO SMALL OMEGA (U+1D71C TO U+1D74E)
math_bold_italic_greek :: String
math_bold_italic_greek = "𝜜𝜝𝜞𝜟𝜠𝜡𝜢𝜣𝜤𝜥𝜦𝜧𝜨𝜩𝜪𝜫𝜬𝜭𝜮𝜯𝜰𝜱𝜲𝜳𝜴"
                         ++ "𝜶𝜷𝜸𝜹𝜺𝜻𝜼𝜽𝜾𝜿𝝀𝝁𝝂𝝃𝝄𝝅𝝆𝝇𝝈𝝉𝝊𝝋𝝌𝝍𝝎"

-- U+1D735	𝜵	f0 9d 9c b5	MATHEMATICAL BOLD ITALIC NABLA
-- U+1D74F	𝝏	f0 9d 9d 8f	MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
-- U+1D750	𝝐	f0 9d 9d 90	MATHEMATICAL BOLD ITALIC EPSILON SYMBOL
-- U+1D751	𝝑	f0 9d 9d 91	MATHEMATICAL BOLD ITALIC THETA SYMBOL
-- U+1D752	𝝒	f0 9d 9d 92	MATHEMATICAL BOLD ITALIC KAPPA SYMBOL
-- U+1D753	𝝓	f0 9d 9d 93	MATHEMATICAL BOLD ITALIC PHI SYMBOL
-- U+1D754	𝝔	f0 9d 9d 94	MATHEMATICAL BOLD ITALIC RHO SYMBOL
-- U+1D755	𝝕	f0 9d 9d 95	MATHEMATICAL BOLD ITALIC PI SYMBOL

-- MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA TO SMALL OMEGA (U+1D756 TO U+1D788)
math_sansserif_bold_greek :: String
math_sansserif_bold_greek = "𝝖𝝗𝝘𝝙𝝚𝝛𝝜𝝝𝝞𝝟𝝠𝝡𝝢𝝣𝝤𝝥𝝦𝝧𝝨𝝩𝝪𝝫𝝬𝝭𝝮"
                            ++ "𝝰𝝱𝝲𝝳𝝴𝝵𝝶𝝷𝝸𝝹𝝺𝝻𝝼𝝽𝝾𝝿𝞀𝞁𝞂𝞃𝞄𝞅𝞆𝞇𝞈"

-- U+1D76F	𝝯	f0 9d 9d af	MATHEMATICAL SANS-SERIF BOLD NABLA
-- U+1D789	𝞉	f0 9d 9e 89	MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
-- U+1D78A	𝞊	f0 9d 9e 8a	MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL
-- U+1D78B	𝞋	f0 9d 9e 8b	MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL
-- U+1D78C	𝞌	f0 9d 9e 8c	MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL
-- U+1D78D	𝞍	f0 9d 9e 8d	MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL
-- U+1D78E	𝞎	f0 9d 9e 8e	MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL
-- U+1D78F	𝞏	f0 9d 9e 8f	MATHEMATICAL SANS-SERIF BOLD PI SYMBOL

-- MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA TO SMALL OMEGA (U+1D790 TO U+1D7C2)
math_sansserif_bold_italic_greek :: String
math_sansserif_bold_italic_greek = "𝞐𝞑𝞒𝞓𝞔𝞕𝞖𝞗𝞘𝞙𝞚𝞛𝞜𝞝𝞞𝞟𝞠𝞡𝞢𝞣𝞤𝞥𝞦𝞧𝞨"
                                   ++ "𝞪𝞫𝞬𝞭𝞮𝞯𝞰𝞱𝞲𝞳𝞴𝞵𝞶𝞷𝞸𝞹𝞺𝞻𝞼𝞽𝞾𝞿𝟀𝟁𝟂"

-- U+1D7A9	𝞩	f0 9d 9e a9	MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
-- U+1D7C3	𝟃	f0 9d 9f 83	MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
-- U+1D7C4	𝟄	f0 9d 9f 84	MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL
-- U+1D7C5	𝟅	f0 9d 9f 85	MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL
-- U+1D7C6	𝟆	f0 9d 9f 86	MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL
-- U+1D7C7	𝟇	f0 9d 9f 87	MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL
-- U+1D7C8	𝟈	f0 9d 9f 88	MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL
-- U+1D7C9	𝟉	f0 9d 9f 89	MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL
-- U+1D7CA	𝟊	f0 9d 9f 8a	MATHEMATICAL BOLD CAPITAL DIGAMMA
-- U+1D7CB	𝟋	f0 9d 9f 8b	MATHEMATICAL BOLD SMALL DIGAMMA

-- MATHEMATICAL BOLD DIGIT ZERO TO NINE (U+1D7CE TO U+1D7D7)
math_bold_digit :: String
math_bold_digit = "𝟎𝟏𝟐𝟑𝟒𝟓𝟔𝟕𝟖𝟗"

-- MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO TO NINE (U+1D7D8 TO U+1D7E1)
math_struck_digit :: String
math_struck_digit = "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡"

-- MATHEMATICAL SANS-SERIF DIGIT ZERO TO NINE (U+1D7E2 TO U+1D7EB)
math_sansserif_digit :: String
math_sansserif_digit = "𝟢𝟣𝟤𝟥𝟦𝟧𝟨𝟩𝟪𝟫"

-- MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO TO NINE (U+1D7EC TO U+1D7F5)
math_sansserif_bold_digit :: String
math_sansserif_bold_digit = "𝟬𝟭𝟮𝟯𝟰𝟱𝟲𝟳𝟴𝟵"

-- MATHEMATICAL MONOSPACE DIGIT ZERO TO NINE (U+1D7F6 TO U+1D7FF)
math_mono_digit :: String
math_mono_digit = "𝟶𝟷𝟸𝟹𝟺𝟻𝟼𝟽𝟾𝟿"

----------------------------------------------------------------------
--                             helpers                              --

trieFromAssocs :: Assocs -> Trie Key String
trieFromAssocs = Trie.fromList . map (first (map keyFromChar))

assocsFromList :: [([String], String)] -> Assocs
assocsFromList arg = do
  (xs, y) <- arg
  x <- xs
  return (x, y)

latex :: String -> String
latex long = '\\':long ++ ";"

