module MagicHaskeller.DataWrangling_Emails where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General

-----------------------------
--- Comprobaciones (Bool) ---



--------------------------------------------
--- Funciones específicas de correos  ------
--------------------------------------------

getWordsBeforeAt :: [Char] -> [Char]
getWordsBeforeAt x = getStartToFirstSymbolOccurrence x at

getWordsAfterAt :: [Char] -> [Char]
getWordsAfterAt x = getLastSymbolOccurrenceToEnd x at

getWordsBeforeDot :: [Char] -> [Char]
getWordsBeforeDot x = getStartToFirstSymbolOccurrence x dot

getWordsAfterDot :: [Char] -> [Char]
getWordsAfterDot x = getLastSymbolOccurrenceToEnd x dot

getWordsBetweenAtAndDot :: [Char] -> [Char]
getWordsBetweenAtAndDot  x = getWordsBeforeDot (getWordsAfterAt x)

appendAt :: [Char] -> [Char]
appendAt x = x++at

prependAt :: [Char] -> [Char]
prependAt x = at++at



--- Juntar dos strings con arroba
joinStringsWithAt :: [Char] -> [Char] -> [Char]
joinStringsWithAt _ [] = []
joinStringsWithAt [] _ = []
joinStringsWithAt x y = x ++ at ++ y
