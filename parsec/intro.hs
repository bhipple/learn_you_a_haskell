import Data.Char
import Text.Parsec

benfun :: Char -> Int
benfun c = ord c - ord 'a'

--charAParser = (Char 'a')
--test p = parse p ""
