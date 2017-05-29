module MagicHaskeller.DataWrangling_Constants where
import Language.Haskell.TH as TH

------------------
--- Constantes ---
------------------

--- Signos de puntuación

dash :: [Char]
dash = "-"

slash :: [Char]
slash = "/"

dot :: [Char]
dot = "."

comma :: [Char]
comma = ","

colon  :: [Char]
colon = ":"

lBracket  :: [Char]
lBracket = "["

rBracket  :: [Char]
rBracket = "]"

and :: [Char]
and = "and"

--- ####
andWithDots :: [Char]
andWithDots = ".and."

at :: [Char]
at = "@"

hash :: [Char]
hash = "#"

lparentheses :: [Char]
lparentheses = "("

rparentheses :: [Char]
rparentheses = ")"

space :: [Char]
space = " "

--- Datos personales
nomenclatureList :: [[Char]]
nomenclatureList = [ "Dr.",
					 "Dra.",
					 "Sr.",
					 "Sra.",
					 "Mr.",
					 "II",
					 "Jr.",
					 "D.",
					 "Prof.",
					 "PhD",
					 "Mrs."
					]

--- Números 
--- Añadidos directamente en individual.hs

--- Números (formato char)

zero  :: [Char]
zero = "0"

nineteen  :: [Char]
nineteen = "19"

twenty  :: [Char]
twenty = "20"

--- Posiciones array de formato/tamaño fijo
firstElement :: [Char]
firstElement = "firstElement"

middleElement :: [Char]
middleElement = "middleElement"

lastElement :: [Char]
lastElement = "lastElement"

--- Constantes de fechas

monthList :: [[Char]]
monthList = [	"January",
		"February",
		"March",
		"April",
		"May",
		"June",
		"July",
		"August",
		"September",
		"October",
		"November",
		"December"]
				
weekDayList :: [[Char]]
weekDayList = [	"Monday",
		"Tuesday",
		"Wednesday",
		"Thursday",
		"Friday",
		"Saturday",
		"Sunday"]

formatDMY :: [Char]
formatDMY = "DMY"

formatDYM :: [Char]
formatDYM = "DYM"

formatMDY :: [Char]
formatMDY = "MDY"

formatMYD :: [Char]
formatMYD = "MYD"

formatYMD :: [Char]
formatYMD = "YMD"

formatYDM :: [Char]
formatYDM = "YDM"

formatDM :: [Char]
formatDM = "DM"

formatMY :: [Char]
formatMY = "MY"

formatDY :: [Char]
formatDY = "DY"

formatMD :: [Char]
formatMD = "MD"

formatYM :: [Char]
formatYM = "YM"

formatYD :: [Char]
formatYD = "YD"

formatD :: [Char]
formatD = "D"

formatM :: [Char]
formatM = "M"

formatY :: [Char]
formatY = "Y"

--- Emails

dotcom :: [Char]
dotcom = ".com"