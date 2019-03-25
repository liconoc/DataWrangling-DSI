import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S
import Data.Array


------------------------------
--- Comprobaciones (Bool) ---
-----------------------------
isNomenclature :: [Char] -> Bool
isNomenclature x 
	| elem x nomenclatureMaleList == True || elem x nomenclatureFemaleList == True = True
--	| length (filter isAlpha (take 1 x)) == 1  && (take 1 (drop 1 x) == dot) = True
	| otherwise = False


----------------------------------------------------
--- Funciones específicas de datos personales  ------
----------------------------------------------------
addMaleNomenclature :: [Char] -> Int -> [Char]
addMaleNomenclature x y = nomenclatureMaleList!!y ++ space ++ x

addFemaleNomenclature :: [Char] -> Int -> [Char]
addFemaleNomenclature x y = nomenclatureFemaleList!!y ++ space ++ x

deleteNomenclature :: [Char] -> [Char]
deleteNomenclature x = unwords (filter (not . isNomenclature) (words x))

getNomenclature :: [Char] -> [Char]
getNomenclature x = unwords (filter isNomenclature (words x))
	
reduceNameSecondWord :: [Char] -> [Char] -> [Char]
reduceNameSecondWord x y = x ++ comma ++ space ++ (toUpperString (take 1 y)) ++ dot

getGenderByNomenclature :: [Char] -> [Char]
getGenderByNomenclature x
	| elem nomenclature nomenclatureMaleList == True && elem nomenclature nomenclatureFemaleList == False = "Male"
	| elem nomenclature nomenclatureMaleList == False && elem nomenclature nomenclatureFemaleList == True = "Female"
	| elem nomenclature nomenclatureMaleList == True && elem nomenclature nomenclatureFemaleList == True = "Indeterminate"
	| otherwise = []
	where nomenclature = getNomenclature x

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
	| (read x::Int)>12 && isDay x && (isShortYear y || isShortYear z || isLongYear y || isLongYear z) = x
	| (read y::Int)>12 && isDay y && (isShortYear x || isShortYear z || isLongYear x || isLongYear z) = y
	| (read z::Int)>12 && isDay z && (isShortYear y || isShortYear x || isLongYear y || isLongYear x) = z
	| isDay x && (isMonth y || isMonth z) = x
	| isDay y && (isMonth x || isMonth z) = y
	| isDay z && (isMonth x || isMonth z) = z
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
	

getWeekDayArray :: [[Char]] -> [Char]
getWeekDayArray (x:xs)
	| elem x weekDayList == True = x
	| otherwise = getWeekDayArray xs
getWeekDayArray _ = []

getWeekDayString :: [Char] -> [Char]
getWeekDayString x = getWeekDayArray (words x)


-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
---- ACTUALIZADO 20/11/2017
---- NO TIENE ENCUENTA ABREVIATURAS
getMonthNameString :: [Char] -> [Char]
getMonthNameString x = getMonthNameArray (words x)

getMonthNameArray :: [[Char]] -> [Char]
getMonthNameArray (x:xs)
	| elem x monthList == True = x
	| otherwise = getMonthNameArray xs
getMonthNameArray _ = []


convertMonth :: [Char] -> [Char]
convertMonth x
	| isNumeric x && val<13 = monthList!!(val-1)
	| month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
	| month /= Nothing && monthNumber < 13 = show monthNumber
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	
		
-- Año

--MagicHaskeller: !!! getYearString:: [Char] -> [Char]
getYearString :: [Char] -> [Char]
getYearString x = getYearArray (splitStringTakeOffPunctuation x)
getYearString _ = []

-- puede sacar un año q no lo sea cuando son cortos < 31 
--- ACTUALIZADO 20/11/2017 Arreglar para detectar años cortos (solo lo detecta bien si es el primer número de la fecha)
getYearArray :: [[Char]] -> [Char]
getYearArray (x:xs)
	| isLongYear x = x
	| isShortYear x = x
	| otherwise = getYearArray xs
getYearArray _ = []

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

-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------

isNumeric :: [Char] -> Bool
isNumeric (x:xs) = (isDigit x) && (isNumeric xs)
isNumeric _ = True



--------------------------------------------
--- Funciones de manipulación de cadenas ---
--------------------------------------------

--- Añade un signo de puntuación aun array
setPunctuationArray :: [[Char]] -> [Char] -> [[Char]]
setPunctuationArray (x:xs) y 
	| xs==[] = [x] ++ setPunctuationArray xs y
	| otherwise = [x] ++ [y] ++ setPunctuationArray xs y
setPunctuationArray [] _ = []

-- Cambiar todos los signos de puntuación en array
changePunctuationArray :: [[Char]] -> [Char] -> [[Char]]
changePunctuationArray [x,_,y,_,z] f = [x] ++ [f] ++ [y] ++ [f] ++ [z] ---Específico fechas
changePunctuationArray (x:xs) (y) 
	| isPunctuation (x!!0) || isSpace (x!!0) = [y] ++ changePunctuationArray xs y
	| otherwise = [x] ++ changePunctuationArray xs y
changePunctuationArray _ _ = []

-- Cambiar todos los signos de puntuación en string
changePunctuationString :: [Char] -> [Char] -> [Char]
changePunctuationString (x:xs) (y) 
	| isPunctuation x  || isSpace x = y ++ changePunctuationString xs y
	| otherwise = [x] ++ changePunctuationString xs y
changePunctuationString _ _ = []

-- Quitar signos de puntuación en array
deletePunctuationArray :: [[Char]] -> [[Char]]
deletePunctuationArray [x,_,y,_,z] = [x] ++ [y] ++ [z] ---Específico fechas
deletePunctuationArray (x:xs) 
	| isPunctuation (x!!0) || isSpace (x!!0)  = [] ++ deletePunctuationArray xs
	| otherwise = [x] ++ deletePunctuationArray xs
deletePunctuationArray _ = []

-- Quitar signos de puntuación en string
deletePunctuationString :: [Char] -> [Char]
deletePunctuationString (x:xs) 
	| isPunctuation x  || isSpace x = [] ++ deletePunctuationString xs
	| otherwise = [x] ++ deletePunctuationString xs
deletePunctuationString _ = []

-- Quitar un signo de puntuacion en concreo en string
deleteSomePunctuationString :: [Char] -> [Char] -> [Char]
deleteSomePunctuationString (x:xs) y
	| [x] == y = [] ++ deleteSomePunctuationString xs y
	| otherwise = [x] ++ deleteSomePunctuationString xs y
deleteSomePunctuationString _ _ = []

-- Separar cadena por un signo concreto (quitando el signo)
splitStringByPunctuation :: [Char] -> [Char] -> [[Char]]
splitStringByPunctuation x y = S.splitOn y x

-- Separar cadena por signos de puntuación (dejando el signo)	
splitStringWithPunctuation :: [Char] -> [[Char]]
splitStringWithPunctuation a = splitOneOf " ,:;_#@-/." a

-- Separar cadena por signos de puntuación (quitando el signo)
splitStringTakeOffPunctuation :: [Char] -> [[Char]]
splitStringTakeOffPunctuation a = deletePunctuationArray $ splitStringWithPunctuation a

-- alterar orden de los elementos de un string
swapElementsString :: Int -> Int -> [Char] -> [Char]
swapElementsString a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list
					
-- alterar orden de los elementos de un array 
swapElementsArray :: Int -> Int -> [[Char]] -> [[Char]]
swapElementsArray a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list	

					
--- ### append a un lugar concreto con constante numerica
appendPositionArray :: [[Char]] -> [Char] -> Int -> [[Char]]
appendPositionArray x y i = x1 ++ [y] ++ x2
	where (x1,x2) = splitAt (i+1) x
	
appendPositionString :: [Char] -> [Char] -> Int -> [Char]
appendPositionString x y i = x1 ++ y ++ x2
	where (x1,x2) = splitAt (i+1) x

--Append el penultimo 
appendNextToLast :: [[Char]] -> [Char] -> [[Char]]
appendNextToLast x y = x1 ++ [y] ++ x2
	where (x1,x2) = splitAt ((length x)-1) x	

-- append algo a un string
append :: [Char] -> [Char] -> [Char]
append x a = x++a

--append en array tamaño fijo
append_first :: [[Char]] -> [Char] -> [[Char]]
append_first [x,y] a = [x++(filter (not . (`elem` "'")) a)]++[y]
append_first [x,y,z] a = [x++(filter (not . (`elem` "'")) a)]++[y]++[z]
append_first [x,s1,y,s2,z] a = [x++(filter (not . (`elem` "'")) a)]++[s1]++[y]++[s2]++[z]
append_first _ _ = []

append_middle :: [[Char]] -> [Char] -> [[Char]]
append_middle [x,y,z] a = [x]++[y++(filter (not . (`elem` "'")) a)]++[z]
append_middle [x,s1,y,s2,z] a  = [x]++[s1]++[y++(filter (not . (`elem` "'")) a)]++[s2]++[z]
append_middle _ _ = []

append_last :: [[Char]] -> [Char] -> [[Char]]
append_last [x,y] a = [x]++[y++(filter (not . (`elem` "'")) a)]
append_last [x,y,z] a = [x]++[y]++[z++(filter (not . (`elem` "'")) a)]
append_last [x,s1,y,s2,z] a = [x]++[s1]++[y]++[s2]++[z++(filter (not . (`elem` "'")) a)]
append_last _ _ = []

		
-- prepend algo a un string
prepend :: [Char] -> [Char] -> [Char]
prepend x a = a++x

--prepend en array tamaño fijo
prepend_first :: [[Char]] -> [Char] -> [[Char]]
prepend_first [x,y] a = [(filter (not . (`elem` "'")) a)++x]++[y]
prepend_first [x,y,z] a = [(filter (not . (`elem` "'")) a)++x]++[y]++[z]
prepend_first [x,s1,y,s2,z] a = [(filter (not . (`elem` "'")) a)++x]++[s1]++[y]++[s2]++[z]
prepend_first _ _ = []

prepend_middle :: [[Char]] -> [Char] -> [[Char]]
prepend_middle [x,y,z] a = [x]++[(filter (not . (`elem` "'")) a)++y]++[z]
prepend_middle [x,s1,y,s2,z] a = [x]++[s1]++[(filter (not . (`elem` "'")) a)++y]++[s2]++[z]
prepend_middle _ _ = []

prepend_last :: [[Char]] -> [Char] -> [[Char]]
prepend_last [x,y] a = [x]++[(filter (not . (`elem` "'")) a)++y]
prepend_last [x,y,z] a = [x]++[y]++[(filter (not . (`elem` "'")) a)++z]
prepend_last [x,s1,y,s2,z] a = [x]++[s1]++[y]++[s2]++[(filter (not . (`elem` "'")) a)++z]
prepend_last _ _ = []

--- Reemplazar un elemento de un array por posición 
replacePositionArray :: [[Char]] -> [Char] -> Int -> [[Char]]
replacePositionArray x y i = a2 ++ [y] ++ b1
	where 	
		(a1,b1) = splitAt (i+1) x
		(a2, b2) = splitAt ((length a1)-1) a1
		
replacePositionString :: [Char] -> [Char] -> Int -> [Char]
replacePositionString x y i = a2 ++ y ++ b1
	where 	
		(a1,b1) = splitAt (i+1) x
		(a2, b2) = splitAt ((length a1)-1) a1
		
-- alterar orden de los elementos de un array de tamaño fijo (como una fecha)
replacePositionArrayFixedSize :: [[Char]] -> [Char] -> [Char] -> [[Char]]
replacePositionArrayFixedSize [x,y] _ _ = [y]++[x]
replacePositionArrayFixedSize [x,y,z] f1 f2
	| f1 == firstElement && f2 == middleElement = [y]++[x]++[z] 
	| f1 == firstElement && f2 == lastElement = [z]++[y]++[x] 
	| f1 == middleElement && f2 == lastElement = [x]++[z]++[y] 
	| otherwise = []
replacePositionArrayFixedSize [x,s1,y,s2,z] f1 f2
	| f1 == firstElement && f2 == middleElement = [y]++[s1]++[x]++[s2]++[z] 
	| f1 == firstElement && f2 == lastElement = [z]++[s1]++[y]++[s2]++[x] 
	| f1 == middleElement && f2 == lastElement = [x]++[s1]++[z]++[s2]++[y] 
	| otherwise = []
replacePositionArrayFixedSize _ _ _ = []
		
replaceAll :: [Char] -> [Char] -> [Char] -> [Char]
replaceAll (x:xs) y z
	| [x] == y = z ++ replaceAll xs y z
	| otherwise = [x] ++ replaceAll xs y z
replaceAll [] _ _ = []

replaceNextToLast :: [[Char]] -> [Char] -> [[Char]]
replaceNextToLast x y = a2 ++ [y] ++ b1
	where 	
		(a1,b1) = splitAt ((length x)-1) x
		(a2, b2) = splitAt ((length a1)-1) a1
	
--- Minusculas
toLowString :: [Char] -> [Char]
toLowString x = T.unpack (T.toLower (T.pack x))

--- Mayusculas 
toUpperString :: [Char] -> [Char]
toUpperString x = T.unpack (T.toUpper (T.pack x))

-- Reducir palabra 
reduceWord :: [Char] -> Int -> [Char]
reduceWord x y = take y x

-- take one element of an array 
takeOneOfArray :: [[Char]] -> Int -> [Char]
takeOneOfArray x y = x!!y

-- take one element of an array
takeOneOfFixedSizeArray :: [[Char]] -> [Char] -> [Char]
takeOneOfFixedSizeArray [x,y] f
	| f == firstElement = x
	| f == lastElement = y
	| otherwise = []
takeOneOfFixedSizeArray [x,y,z] f
	| f == firstElement = x
	| f == middleElement = y
	| f == lastElement = z
	| otherwise = []
takeOneOfFixedSizeArray [x,s1,y,s2,z] f 
	| f == firstElement = x 
	| f == middleElement = y
	| f == lastElement = z
	| otherwise = []
takeOneOfFixedSizeArray (x:xs) _ = x
takeOneOfFixedSizeArray _ _ = []

--take one de un string con tamaño prefijado por puntuaciones (como una fecha)
takeOneOfFixedSizeString :: [Char] -> [Char] -> [Char]
takeOneOfFixedSizeString x f 
	| f == firstElement = takeOneOfFixedSizeArray (splitStringTakeOffPunctuation x) firstElement
	| f == middleElement = takeOneOfFixedSizeArray (splitStringTakeOffPunctuation x) middleElement
	| f == lastElement = takeOneOfFixedSizeArray (splitStringTakeOffPunctuation x) lastElement
	|otherwise = []
takeOneOfFixedSizeString _ _ = []


--- Juntar dos strings con un espacio o signo de puntuación (si hay dos, dos, si hay una una...  nada.)
joinStringsWithPunctuation :: [Char] -> [Char] -> [Char] -> [Char]
joinStringsWithPunctuation _ [] _ = []
joinStringsWithPunctuation [] _ _ = []
joinStringsWithPunctuation x y f = x ++ f ++ y

--- Ejemplos de List od APIS
getOneWordByPosition :: [Char] -> Int -> [Char]
getOneWordByPosition x y = wrd!!y	
	where wrd = words x
	
getFirstWord :: [Char] -> [Char]
getFirstWord x = getOneWordByPosition x 0

getLastWord :: [Char] -> [Char]
getLastWord x = getOneWordByPosition x (s-1)
	where s = length (words x)

getOneCharacterByPosition :: [Char] -> Int -> [Char]
getOneCharacterByPosition x y = take 1 (drop y x)	

getFirstCharacter :: [Char] -> [Char]
getFirstCharacter x = getOneCharacterByPosition x 0

getLastCharacter :: [Char] -> [Char]
getLastCharacter x = getOneCharacterByPosition x ((length x)-1)

getStartToFirstSymbolOccurrence :: [Char] -> [Char] -> [Char]
getStartToFirstSymbolOccurrence x y = (splitStringByPunctuation x y)!!0

getStartToLastSymbolOccurrence :: [Char] -> [Char] -> [Char]
getStartToLastSymbolOccurrence x y = intercalate y (take (l-1) wrd)
	where 
	wrd = splitStringByPunctuation x y
	l = length wrd

getLastSymbolOccurrenceToEnd :: [Char] -> [Char] -> [Char]
getLastSymbolOccurrenceToEnd x y = wrd!!((length wrd)-1)
	where wrd = splitStringByPunctuation x y

getFirstSymbolOccurrenceToEnd :: [Char] -> [Char] -> [Char]
getFirstSymbolOccurrenceToEnd x y = intercalate y (drop 1 (splitStringByPunctuation x y))

----- NEW 22/05/2017 -----
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

-- take one element of an array 
takeTwoOfThreeArray :: [[Char]] -> Int -> Int -> [[Char]]
takeTwoOfThreeArray x y z = [x!!y] ++ [x!!z]

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

--getDayOrdinal en un array.. no hay diferencia en fisrt, second, third. todo th
convertDayOrdinalWithinArray :: [[Char]] -> [[Char]]
convertDayOrdinalWithinArray  (x:xs)
	| isNumeric x && isDay x = [x++"th"] ++ xs
	| otherwise = [x] ++ convertDayOrdinalWithinArray xs
convertDayOrdinalWithinArray _ = []

--- ACTUALIZADO 20/11/2017 No hay uno paraalargar el año.

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

-- Reducir string mes (January -> Jan o Jan.)
reduceMonth :: [Char] -> [Char]
reduceMonth x = reduceWord x 3

----- NEW 24/05/2017 -----

--- Añade un signo de puntuación aun array y genera string
joinArrayWithPunctuation :: [[Char]] -> [Char] -> [Char]
joinArrayWithPunctuation (x:xs) y 
	| xs==[] = concat([x] ++ setPunctuationArray xs y)
	| otherwise = concat([x] ++ [y] ++ setPunctuationArray xs y)
joinArrayWithPunctuation [] _ = []

appendAt :: [Char] -> [Char]
appendAt x = x++at

prependAt :: [Char] -> [Char]
prependAt x = at++at

dotcom :: [Char]
dotcom = ".com"

--- Juntar dos strings con arroba
joinStringsWithAt :: [Char] -> [Char] -> [Char]
joinStringsWithAt _ [] = []
joinStringsWithAt [] _ = []
joinStringsWithAt x y = x ++ at ++ y

joinStringsWithoutPunctuation :: [Char] -> [Char] -> [Char]
joinStringsWithoutPunctuation _ [] = []
joinStringsWithoutPunctuation [] _ = []
joinStringsWithoutPunctuation x y = x ++ y

setParentheses :: [Char] -> [Char]
setParentheses x = lparentheses ++ x ++ rparentheses

--------- NEW 25/05/2017 -----
deleteNomenclatureAndPunctuation :: [Char] -> [Char]
deleteNomenclatureAndPunctuation x = deleteSomePunctuationString (deleteSomePunctuationString (deleteNomenclature x) comma) dot

--- Datos personales
nomenclatureFemaleList :: [[Char]]
nomenclatureFemaleList = [ "Dra.",
					 "Sra.",
					 "Prof.",
					 "PhD",
					 "Mrs."
					]
					
nomenclatureMaleList :: [[Char]]
nomenclatureMaleList = [ "Dr.",
					 "Sr.",
					 "Mr.",
					 "II",
					 "Jr.",
					 "D.",
					 "Prof.",
					 "PhD"
					]

reduceNamesFirstPlace :: [Char] -> [Char]
reduceNamesFirstPlace x
	| len == 2 = (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot ++ space ++ (unwords (drop 1 wrd))
	| len > 2 = (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot ++ space ++ (toUpperString (take 1 ((take 1 (drop 1 wrd))!!0))) ++ dot ++ space ++ (unwords (drop 2 wrd))
	| otherwise = reduceNameFirstPlace x
	where 	
		wrd = (filter (not . isNomenclature) (words x))
		len = length wrd
		
reduceNameFirstPlace :: [Char] -> [Char]
reduceNameFirstPlace x
	| len == 2 = (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot ++ space ++ (unwords (drop 1 wrd))
	| len > 2 = (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot ++ space ++ unwords (take 1 (drop 2 wrd))
	| otherwise = []
	where 	
		wrd = (filter (not . isNomenclature) (words x))
		len = length wrd
	
reduceNameWithSurnameSecondPlace :: [Char] -> [Char]
reduceNameWithSurnameSecondPlace x 
	| len < 3 = (wrd!!1) ++ comma ++ space ++ (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot
	| len == 3 = (wrd!!2) ++ comma ++ space ++ (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot
	| len > 3 = reduceNameWithSurnamesSecondPlace x
	where 
		wrd = (filter (not . isNomenclature) (words x))
		len = length wrd
	
reduceNameWithSurnamesSecondPlace :: [Char] -> [Char]
reduceNameWithSurnamesSecondPlace x
	| len == 3 = (wrd!!1) ++ space ++ (wrd!!2) ++ comma ++ space ++ (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot
	| len > 3 = (wrd!!2) ++ space ++ (wrd!!3) ++ comma ++ space ++ (toUpperString (take 1 ((take 1 wrd)!!0))) ++ dot ++ space ++ (toUpperString (take 1 ((drop 1 (take 2 wrd))!!0))) ++ dot
	| otherwise = []
	where 
		wrd = (filter (not . isNomenclature) (words x))
		len = length wrd
		
initialsNameFirstPlace :: [Char] -> [Char]
initialsNameFirstPlace x = getFirstWord wrd ++ getFirstCharacter(getLastWord wrd) ++ dot
	where wrd = reduceNameFirstPlace x
	
----- NEW 26/05/2017 -----
getCaps :: [Char] -> [Char]
getCaps x = filter isUpper x

reduceSpaces :: [Char] -> [Char]
reduceSpaces x = unwords (words x)

setBrackets :: [Char] -> [Char]
setBrackets x = lBracket ++ x ++ rBracket

completeBrackets :: [Char] -> [Char]
completeBrackets x
	| l == lBracket && r == rBracket = x
	| l == lBracket && r /= rBracket = x ++ rBracket
	| l /= lBracket && r == rBracket = lBracket ++ x
	| otherwise = setBrackets x
	where
		l = getFirstCharacter x
		r = getLastCharacter x
		
completeParentheses :: [Char] -> [Char]
completeParentheses x
	| l == lparentheses && r == rparentheses = x
	| l == lparentheses && r /= rparentheses = x ++ rparentheses
	| l /= lparentheses && r == rparentheses = lparentheses ++ x
	| otherwise = setParentheses x
	where
		l = getFirstCharacter x
		r = getLastCharacter x
		
getFirstDigitToEnd :: [Char] -> [Char]
getFirstDigitToEnd (x:xs)
	| isDigit x = [x] ++ xs
	| otherwise = getFirstDigitToEnd xs
getFirstDigitToEnd _ = []

getStartToFirstDigit :: [Char] -> [Char]
getStartToFirstDigit (x:xs)
	| isDigit x = []
	| otherwise = [x] ++ getStartToFirstDigit xs
getStartToFirstDigit _ = []