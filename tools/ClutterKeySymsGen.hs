
-- This probably isn't necessary and rather dumb, but whatever.  Would
-- probably be served better by hsc2hs or something, but these
-- shouldn't change anyway

import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe

header = "/usr/include/clutter-1.0/clutter/clutter-keysyms.h"

prefix = "#define CLUTTER_"



usToCC [] = []
usToCC ('_':c:cs) = toUpper c:usToCC cs
usToCC (c:cs)     = c:usToCC cs

good = filter (isPrefixOf prefix) . lines

strip = catMaybes . map (stripPrefix prefix)

--firstUp (c:cs) = toLower c:cs

convert = concatMap (\[n,v] -> toCode ('k':n) v)

toCode n v = unlines [ n ++ " :: KeySym",
                       n ++ " = " ++ v,
                       " "]

fileHeader = "module Graphics.UI.Clutter.KeySyms where\n\n\
             \import Data.Word\n\n\
             \type KeySym = Word32\n\n"

outFile cont = writeFile "KeySyms.hs" (fileHeader ++ cont)

main = outFile =<< convert . map words . strip . good <$> readFile header


