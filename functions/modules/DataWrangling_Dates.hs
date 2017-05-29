module MagicHaskeller.DataWrangling_Dates where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import MagicHaskeller.DataWrangling_Strings
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General


-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------
isDay :: [Char] -> Bool
isDay x 
	| 1 <= val && val <= 31 = True
	| otherwise = False
	where val = read x :: Int
	
isMonth :: [Char] -> Bool
isMonth x 
	| elem x monthList == True = True
	| isNumeric x && 1 <= val && val <= 12 = True
	| otherwise = False
	where val = read x :: Int
	
isShortYear :: [Char] -> Bool
isShortYear x 
	| 0 <= val && val <= 99 = True
	| otherwise = False
	where val = read x :: Int
	
isLongYear :: [Char] -> Bool
isLongYear x 
	| isNumeric x && length x == 4 = True
	| otherwise = False
	
isDMY :: [[Char]] -> Bool
isDMY [d,_,m,_,y]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDMY [d,m,y] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDMY _ = False

isDYM :: [[Char]] -> Bool
isDYM [d,_,y,_,m]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDYM [d,y,m] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDYM _ = False

isMDY :: [[Char]] -> Bool
isMDY [m,_,d,_,y]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMDY [m,d,y] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMDY _ = False

isMYD :: [[Char]] -> Bool
isMYD [m,_,y,_,d]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMYD [m,y,d] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMYD _ = False

isYMD :: [[Char]] -> Bool
isYMD [y,_,m,_,d]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYMD [y,m,d] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYMD _ = False

isYDM :: [[Char]] -> Bool
isYDM [y,_,d,_,m]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYDM [y,d,m] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYDM _ = False

isDM :: [[Char]] -> Bool
isDM [d,m] 
	| isDay d && isMonth m = True
	| otherwise = False
isDM _ = False

isMD :: [[Char]] -> Bool
isMD [m,d] 
	| isDay d && isMonth m = True
	| otherwise = False
isMD _ = False

isDY :: [[Char]] -> Bool
isDY [d,y] 
	| isDay d && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDY _ = False

isMY :: [[Char]] -> Bool
isMY [m,y] 
	| isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMY _ = False

isYM :: [[Char]] -> Bool
isYM [y,m] 
	| isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYM _ = False

isYD :: [[Char]] -> Bool
isYD [y,d] 
	| isDay d && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYD _ = False

isWeekDay :: [Char] -> Bool
isWeekDay x 
	| elem x weekDayList == True = True
	|otherwise = False

--------------------------------------------
--- Funciones de manipulación de cadenas ---
--------------------------------------------

--- ######
-- Añadir signos de puntuación en string (6 cifras, 8 cifras, con palabras)
-- Se suponen 0 en las cifras de 2 digitos
addPunctuationString :: [Char] -> [Char] -> [[Char]]
addPunctuationString a f
	| length a == 6 = [take 2 a] ++ [f] ++ [take 2 (drop 2 a)] ++ [f] ++ [take 2 (drop 4 a)]
	| length a == 8 && ( (isDay (take 2 (drop 4 a)) && isMonth (take 2 (drop 6 a))) || (isDay (take 2 (drop 6 a)) && isMonth (take 2 (drop 4 a))) ) && isLongYear (take 4 a) = [take 4 a] ++ [f] ++ [take 2 (drop 4 a)] ++ [f] ++ [take 2 (drop 6 a)]
	| length a == 8 && ( (isDay (take 2 a) && isMonth (take 2 (drop 2 a))) || (isDay (take 2 (drop 2 a)) && isMonth (take 2 a)) ) && isLongYear (take 4 (drop 4 a)) = [take 2 a] ++ [f] ++ [take 2 (drop 2 a)] ++ [f] ++ [take 4 (drop 4 a)]
	| otherwise = []
	
-- Separar cadena sin signos de puntuación (6 cifras, 8 cifras, con palabras)
-- Se suponen 0 en las cifras de 2 digitos
splitStringWithoutPunctuation :: [Char] -> [[Char]]
splitStringWithoutPunctuation a
	| length a == 6 = [take 2 a] ++ [take 2 (drop 2 a)] ++ [take 2 (drop 4 a)]
	| length a == 8 && ( (isDay (take 2 (drop 4 a)) && isMonth (take 2 (drop 6 a))) || (isDay (take 2 (drop 6 a)) && isMonth (take 2 (drop 4 a))) ) && isLongYear (take 4 a) = [take 4 a] ++ [take 2 (drop 4 a)] ++ [take 2 (drop 6 a)]
	| length a == 8 && ( (isDay (take 2 a) && isMonth (take 2 (drop 2 a))) || (isDay (take 2 (drop 2 a)) && isMonth (take 2 a)) ) && isLongYear (take 4 (drop 4 a)) = [take 2 a] ++ [take 2 (drop 2 a)] ++ [take 4 (drop 4 a)]
	| otherwise = []


--------------------------------------------
----- Funciones específicas de fechas ------ ###################
--------------------------------------------

-- Día

-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
-- puede extraer un dia, siendo otro el día, si los dos o los tres son < 12
getDayCardinalString :: [Char] -> [Char]
getDayCardinalString x = getDayCardinalArray (splitStringTakeOffPunctuation x)

getDayCardinalArray :: [[Char]] -> [Char]
getDayCardinalArray [x,y]
	| isDay x && (isMonth y || isLongYear y || isShortYear y) = x
	| isDay y && (isMonth x || isLongYear x || isShortYear x) = y
	| isDay x = x
	| isDay y = y
	|otherwise = []
getDayCardinalArray [x,y,z]
	| isDay x && (isMonth y || isLongYear y || isShortYear y) && (isMonth z || isLongYear z || isShortYear z) = x
	| isDay y && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = y
	| isDay z && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = z
	| isDay x = x
	| isDay y = y
	| isDay z = z
	|otherwise = []
getDayCardinalArray [x,_,y,_,z]
	| isDay x && (isMonth y || isLongYear y || isShortYear y) && (isMonth z || isLongYear z || isShortYear z) = x
	| isDay y && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = y
	| isDay z && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = z
	| isDay x = x
	| isDay y = y
	| isDay z = z
	|otherwise = []
getDayCardinalArray (x:xs)
	| isDay x = x
	| otherwise = getDayCardinalArray xs
getDayCardinalArray _ = []

--getDayOrdinal :: [Char] -> [Char]
getDayOrdinal :: [Char] -> [Char]
getDayOrdinal x 
	| day == [] = []
	| day == "01" || day == "1" = day ++ "st"
	| day == "02" || day == "2" = day ++ "nd"
	| day == "03" || day == "3" = day ++ "rd"
	| otherwise = day ++ "th"
	where day = getDayCardinalString x	

--getDayOrdinal en un array.. no hay diferencia en fisrt, second, third. todo th
convertDayOrdinalWithinArray :: [[Char]] -> [[Char]]
convertDayOrdinalWithinArray  (x:xs)
	| isNumeric x && isDay x = [x++"th"] ++ xs
	| otherwise = [x] ++ convertDayOrdinalWithinArray xs
convertDayOrdinalWithinArray _ = []	

getWeekDayArray :: [[Char]] -> [Char]
getWeekDayArray (x:xs)
	| elem x weekDayList == True = x
	| otherwise = getWeekDayArray xs
getWeekDayArray _ = []

getWeekDayString :: [Char] -> [Char]
getWeekDayString x = getWeekDayArray (words $ deletePunctuationString x)

-- Mes
getMonthArray :: [[Char]] -> [Char]
getMonthArray [x,y]
	| isMD [x,y] == True = x
	| isDM [x,y] == True = y
	| isYM [x,y] == True = y
	| isMY [x,y] == True = x
	| otherwise = []
getMonthArray [x,y,z]
	| isMDY [x,y,z] == True = x
	| isMYD [x,y,z] == True = x
	| isDMY [x,y,z] == True = y
	| isDYM [x,y,z] == True = z
	| isYMD [x,y,z] == True = y
	| isYDM [x,y,z] == True = z
	|otherwise = []
getMonthArray (x:xs)
	| isMonth x = x
	| otherwise = getMonthArray xs
getMonthArray _ = []

getMonthString :: [Char] -> [Char]
getMonthString x = getMonthArray (splitStringTakeOffPunctuation x)



-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
getMonthNameString :: [Char] -> [Char]
getMonthNameString x = getMonthNameArray (words $ deletePunctuationString x)

getMonthNameArray :: [[Char]] -> [Char]
getMonthNameArray (x:xs)
	| elem x monthList == True = x
	| otherwise = getMonthNameArray xs
getMonthNameArray _ = []


convertMonthToNumeric :: [Char] -> [Char]
convertMonthToNumeric x
	| isNumeric x && val<13 = x
	| month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
	| month /= Nothing && monthNumber < 13 = show monthNumber
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	
		
convertMonthToString :: [Char] -> [Char]
convertMonthToString x
	| isNumeric x && val<13 = monthList!!(val-1)
	| month /= Nothing && monthNumber < 13 = x
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	

convertMonthToNumericWithinArray :: [[Char]] -> [[Char]]
convertMonthToNumericWithinArray [x,y]
	| isMD [x,y] == True = [convertMonthToNumeric x] ++ [y]
	| isDM [x,y] == True = [x]++[convertMonthToNumeric y]
	| isYM [x,y] == True = [x]++[convertMonthToNumeric y]
	| isMY [x,y] == True = [convertMonthToNumeric x] ++ [y]
	| otherwise = []
convertMonthToNumericWithinArray [x,y,z]
	| isMDY [x,y,z] == True = [convertMonthToNumeric x]++[y]++[z]
	| isMYD [x,y,z] == True = [convertMonthToNumeric x]++[y]++[z]
	| isDMY [x,y,z] == True = [x]++[convertMonthToNumeric y]++[z]
	| isDYM [x,y,z] == True = [x]++[y]++[convertMonthToNumeric z]
	| isYMD [x,y,z] == True = [x]++[convertMonthToNumeric y]++[z]
	| isYDM [x,y,z] == True = [x]++[y]++[convertMonthToNumeric z]
convertMonthToNumericWithinArray  (x:xs)
	| isMonth x = [convertMonthToNumeric x] ++ xs
	| otherwise = [x] ++ convertMonthToNumericWithinArray xs
convertMonthToNumericWithinArray _ = []

convertMonthToStringWithinArray :: [[Char]] -> [[Char]]
convertMonthToStringWithinArray [x,y]
	| isMD [x,y] == True = [convertMonthToString x] ++ [y]
	| isDM [x,y] == True = [x]++[convertMonthToString y]
	| isYM [x,y] == True = [x]++[convertMonthToString y]
	| isMY [x,y] == True = [convertMonthToString x] ++ [y]
	| otherwise = []
convertMonthToStringWithinArray [x,y,z]
	| isMDY [x,y,z] == True = [convertMonthToString x]++[y]++[z]
	| isMYD [x,y,z] == True = [convertMonthToString x]++[y]++[z]
	| isDMY [x,y,z] == True = [x]++[convertMonthToString y]++[z]
	| isDYM [x,y,z] == True = [x]++[y]++[convertMonthToString z]
	| isYMD [x,y,z] == True = [x]++[convertMonthToString y]++[z]
	| isYDM [x,y,z] == True = [x]++[y]++[convertMonthToString z]
convertMonthToStringWithinArray  (x:xs)
	| isMonth x = [convertMonthToString x] ++ xs
	| otherwise = [x] ++ convertMonthToStringWithinArray xs
convertMonthToStringWithinArray _ = []

-- Reducir string mes (January -> Jan o Jan.)
reduceMonthWithinArray :: [[Char]] -> [[Char]]	
reduceMonthWithinArray  (x:xs)
	| isMonth x = [reduceMonth ([x]!!0)] ++ xs
	| otherwise = [x] ++ reduceMonthWithinArray xs
reduceMonthWithinArray _ = []


-- Reducir string mes (January -> Jan o Jan.)
reduceMonth :: [Char] -> [Char]
reduceMonth x = reduceWord x 3
		
-- Año

--MagicHaskeller: !!! getYearString:: [Char] -> [Char]
getYearString :: [Char] -> [Char]
getYearString x = getYearArray (splitStringTakeOffPunctuation x)
getYearString _ = []

-- puede sacar un año q no lo sea cuando son cortos < 31
getYearArray :: [[Char]] -> [Char]
getYearArray (x:xs)
	| isLongYear x = x
	| otherwise = getYearArray xs
getYearArray _ = []

reduceYear :: [Char] -> [Char]
reduceYear x
	| (isNumeric x) && (isLongYear x) = drop 2 x
	| (isNumeric x) && (isShortYear x) = x
	| otherwise = []
	
reduceYearWithinArray :: [[Char]] -> [[Char]]
reduceYearWithinArray  (x:xs)
	| (isNumeric x) && (isLongYear x) = [reduceYear x] ++ xs
	| otherwise = [x] ++ reduceYearWithinArray xs
reduceYearWithinArray _ = []

changeDateFormat :: [Char] -> [Char] -> [[Char]]
	-- DMY
changeDateFormat x "DMY"
	| isDMY date = [x]
	| isDYM date = [date!!0]++[date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!0]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]++[date!!1]
	| isYMD date = [date!!2]++[date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!2]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- DYM
changeDateFormat x "DYM"
	| isDYM date = [x]
	| isDMY date = [date!!0]++[date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!2]++[date!!0]
	| isMYD date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!2]++[date!!0]++[date!!1]
	| isYDM date = [date!!1]++[date!!0]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- MDY
changeDateFormat x "MDY"
	| isMDY date = [x]
	| isDMY date = [date!!1]++[date!!0]++[date!!2]
	| isDYM date = [date!!2]++[date!!0]++[date!!1]
	| isMYD date = [date!!0]++[date!!2]++[date!!1]
	| isYMD date = [date!!1]++[date!!2]++[date!!0]
	| isYDM date = [date!!2]++[date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YMD
changeDateFormat x "YMD"
	| isYMD date = [x]
	| isDMY date = [date!!2]++[date!!1]++[date!!0]
	| isDYM date = [date!!1]++[date!!2]++[date!!0]
	| isMYD date = [date!!1]++[date!!0]++[date!!2]
	| isMDY date = [date!!2]++[date!!0]++[date!!1]
	| isYDM date = [date!!0]++[date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- MYD
changeDateFormat x "MYD"
	| isMYD date = [x]
	| isDMY date = [date!!1]++[date!!2]++[date!!0]
	| isDYM date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!1]++[date!!0]++[date!!2]
	| isMDY date = [date!!0]++[date!!2]++[date!!1]
	| isYDM date = [date!!2]++[date!!0]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YDM
changeDateFormat x "YDM"
	| isYDM date = [x]
	| isDMY date = [date!!1]++[date!!2]++[date!!0]
	| isDYM date = [date!!1]++[date!!0]++[date!!2]
	| isMYD date = [date!!1]++[date!!2]++[date!!0]
	| isMDY date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!0]++[date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- DM
changeDateFormat x "DM"
	| isDM date = [x]
	| isMD date = [date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!2]
	| isDMY date = [date!!0]++[date!!1]
	| isDYM date = [date!!0]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]
	| isMDY date = [date!!1]++[date!!0]
	| isYMD date = [date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- DY
changeDateFormat x "DY"
	| isDY date = [x]
	| isYD date = [date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!0]
	| isDMY date = [date!!0]++[date!!2]
	| isDYM date = [date!!0]++[date!!1]
	| isMYD date = [date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!2]
	| isYMD date = [date!!2]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- MD
changeDateFormat x "MD"
	| isMD date = [x]
	| isDM date = [date!!1]++[date!!0]
	| isYDM date = [date!!2]++[date!!1]
	| isDMY date = [date!!1]++[date!!0]
	| isDYM date = [date!!2]++[date!!0]
	| isMYD date = [date!!0]++[date!!2]
	| isMDY date = [date!!0]++[date!!1]
	| isYMD date = [date!!1]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- MY
changeDateFormat x "MY"
	| isMY date = [x]
	| isYM date = [date!!1]++[date!!0]
	| isYDM date = [date!!2]++[date!!0]
	| isDMY date = [date!!1]++[date!!2]
	| isDYM date = [date!!2]++[date!!1]
	| isMYD date = [date!!0]++[date!!1]
	| isMDY date = [date!!0]++[date!!2]
	| isYMD date = [date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- YM
changeDateFormat x "YM"
	| isYM date = [x]
	| isMY date = [date!!1]++[date!!0]
	| isYDM date = [date!!0]++[date!!2]
	| isDMY date = [date!!2]++[date!!1]
	| isDYM date = [date!!1]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]
	| isMDY date = [date!!0]++[date!!2]
	| isYMD date = [date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YD
changeDateFormat x "YD"
	| isYD date = [x]
	| isDY date = [date!!1]++[date!!0]
	| isYDM date = [date!!0]++[date!!1]
	| isDMY date = [date!!2]++[date!!0]
	| isDYM date = [date!!1]++[date!!0]
	| isMYD date = [date!!1]++[date!!2]
	| isMDY date = [date!!2]++[date!!1]
	| isYMD date = [date!!0]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- D
changeDateFormat x "D"
	| isDM date = [date!!0]
	| isDY date = [date!!0]
	| isMD date = [date!!1]
	| isYD date = [date!!1]
	| isYDM date = [date!!1]
	| isDMY date = [date!!0]
	| isDYM date = [date!!0]
	| isMYD date = [date!!2]
	| isMDY date = [date!!1]
	| isYMD date = [date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- M
changeDateFormat x "M"
	| isDM date = [date!!1]
	| isMY date = [date!!0]
	| isMD date = [date!!0]
	| isYM date = [date!!1]
	| isYDM date = [date!!2]
	| isDMY date = [date!!1]
	| isDYM date = [date!!2]
	| isMYD date = [date!!0]
	| isMDY date = [date!!0]
	| isYMD date = [date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- Y
changeDateFormat x "Y"
	| isDY date = [date!!1]
	| isMY date = [date!!1]
	| isYD date = [date!!0]
	| isYM date = [date!!0]
	| isYDM date = [date!!0]
	| isDMY date = [date!!2]
	| isDYM date = [date!!1]
	| isMYD date = [date!!1]
	| isMDY date = [date!!2]
	| isYMD date = [date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
