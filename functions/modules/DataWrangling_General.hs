module MagicHaskeller.DataWrangling_General where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S

------------------
--- General ------
------------------

-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------

isNumeric :: [Char] -> Bool
isNumeric (x:xs) = (isDigit x) && (isNumeric xs)
isNumeric _ = True


