module MagicHaskeller.DataWrangling_Words where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S
import Data.Array
import Text.Regex.PCRE
import qualified Data.Map as Map
import Data.Maybe
import MagicHaskeller.DataWrangling_General
import MagicHaskeller.DataWrangling_Constants


-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------

------------ 04/01/2018

isTimeElement :: [Char] -> Bool
isTimeElement x 
	| elem x timesList == True = True
	| otherwise = False

isTime :: [Char] -> Bool
isTime x
	| (any isNumber x && isSubsequenceOf colon x) || (any isTimeElement (words x)) || (any isTimeElement (words (filter isAlpha x))) = True
	| otherwise = False
	
------ New 08/01/2018

isUTC :: [Char] -> Bool
isUTC x
	| isSubsequenceOf utc x == True = True
	| otherwise = False

isCST :: [Char] -> Bool
isCST x
	| isSubsequenceOf cst x == True = True
	| otherwise = False

isPST :: [Char] -> Bool
isPST x
	| isSubsequenceOf pst x == True = True
	| otherwise = False

isEST :: [Char] -> Bool
isEST x
	| isSubsequenceOf est x == True = True
	| otherwise = False

isGMT:: [Char] -> Bool
isGMT x
	| isSubsequenceOf gmt x == True = True
	| otherwise = False

isMST :: [Char] -> Bool
isMST x
	| isSubsequenceOf mst x == True = True
	| otherwise = False

--- 
--- funciones a implementar
--- 

isHM :: [Char] -> Bool
isHM x
	| isTime x && (x =~ "^(\\d{1,2}(:)\\d{1,2})$" :: Bool) == True = True
	| otherwise = False
	
containsHM :: [Char] -> Bool
containsHM x
	| isTime x && (x =~ "([ ]|^)\\d{1,2}(:)\\d{1,2}([ ]|$)" :: Bool) == True = True
	| otherwise = False


isHMS :: [Char] -> Bool
isHMS x 
	| isTime x && (x =~ "^(\\d{1,2}(:)\\d{1,2}(:)\\d{1,2})$" :: Bool) == True = True
	| otherwise = False
	
containsHMS :: [Char] -> Bool
containsHMS x
	| isTime x && (x =~ "([ ]|^)\\d{1,2}(:)\\d{1,2}(:)\\d{1,2}([ ]|$)" :: Bool) == True = True
	| otherwise = False

isH :: [Char] -> Bool
isH x 
	| isTime x && (x =~ "^\\d{1,2}([ ])*(pm|am|PM|AM|h|hr)$" :: Bool) == True = True
	| isTime x && (x =~ "^(\\d{1,2})$" :: Bool) == True = True
	| otherwise = False

isPM :: [Char] -> Bool
isPM x
	| (x =~ "[ ]*(pm|PM)([ ]|$)" :: Bool) == True = True
	| otherwise = False
	
isAM :: [Char] -> Bool
isAM x
	| (x =~ "[ ]*(am|AM)([ ]|$)" :: Bool) == True = True
	| otherwise = False

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
append_first [w,x,y,z] a = [w++(filter (not . (`elem` "'")) a)]++[x]++[y]++[z]
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

----- NEW 24/05/2017 -----

--- Añade un signo de puntuación aun array y genera string
joinArrayWithPunctuation :: [[Char]] -> [Char] -> [Char]
joinArrayWithPunctuation (x:xs) y 
	| xs==[] = concat([x] ++ setPunctuationArray xs y)
	| otherwise = concat([x] ++ [y] ++ setPunctuationArray xs y)
joinArrayWithPunctuation [] _ = []

joinStringsWithoutPunctuation :: [Char] -> [Char] -> [Char]
joinStringsWithoutPunctuation _ [] = []
joinStringsWithoutPunctuation [] _ = []
joinStringsWithoutPunctuation x y = x ++ y

setParentheses :: [Char] -> [Char]
setParentheses x = lparentheses ++ x ++ rparentheses

----- NEW 26/05/2017 -----
getCaps :: [Char] -> [Char]
getCaps x = filter isUpper x

reduceSpaces :: [Char] -> [Char]
reduceSpaces x = deleteSomePunctuationString x space

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

---- Poner esto en otros modulos. No lo hago ahora porque no tengo tiempo (IJCAI2018)
--- 
--- funciones a implementar
--- 
--- un número solo no es time
getTime :: [Char] -> [Char]
getTime x
	| isTime x && containsHMS x = (x =~ "\\d{1,2}(:)\\d{1,2}(:)\\d{1,2}" :: [Char])
	| isTime x && containsHM x = (x =~ "\\b\\d{1,2}(:)\\d{1,2}\\b" :: [Char])
	| isTime x && isH x = x	
	| otherwise = []

getHour :: [Char] -> [Char]
getHour x
	| isTime x && containsHMS x = (splitStringByPunctuation (x =~ "\\d{1,2}(:)\\d{1,2}(:)\\d{1,2}" :: [Char]) colon)!!0
	| isTime x && containsHM x = (splitStringByPunctuation (x =~ "\\b\\d{1,2}(:)\\d{1,2}\\b" :: [Char]) colon)!!0
	| isTime x && isH x = filter isNumber x	
	| otherwise = []

getMinutes :: [Char] -> [Char]
getMinutes x
	| isTime x && containsHMS x = (splitStringByPunctuation (x =~ "\\d{1,2}(:)\\d{1,2}(:)\\d{1,2}" :: [Char]) colon)!!1
	| isTime x && containsHM x = (splitStringByPunctuation (x =~ "\\b\\d{1,2}(:)\\d{1,2}\\b" :: [Char]) colon)!!1
	| otherwise = []

getSeconds :: [Char] -> [Char]
getSeconds x
	| isTime x && containsHMS x = (splitStringByPunctuation (x =~ "\\d{1,2}(:)\\d{1,2}(:)\\d{1,2}" :: [Char]) colon)!!2
	| otherwise = []
	


appendOclockTime :: [Char] -> [Char]
appendOclockTime x
	| isTime x && isHM x = x ++ oclock
	| isTime x && isH x = (filter isNumber x) ++ oclock
	| all isNumber x = x ++ oclock
	| otherwise = []
	
appendSomeTime :: [Char] -> Int -> [Char]
appendSomeTime x t
	| isTime x && isHM x && t<10 = x ++ colon ++ zero ++ (show t)
	| isTime x && isHM x = x ++ colon ++ (show t)
	| isTime x && isH x && t<10 = (filter isNumber x) ++ colon ++ zero ++ (show t)
	| isTime x && isH x = (filter isNumber x) ++ colon ++ (show t)
	| all isNumber x && t<10 = x ++ colon ++ (show t)
	| all isNumber x = x ++ colon ++ zero ++ (show t)
	| otherwise = []


changeHour :: [Char] -> Int -> [Char]
changeHour x t
	| isTime x && isHMS x && t<10 = zero ++ (show t) ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHMS x =(show t) ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHM x && t<10 = zero ++ (show t) ++ colon ++ getMinutes x
	| isTime x && isHM x = (show t) ++ colon ++ getMinutes x
	| isTime x && isH x && t<10 = zero ++ (show t)
	| isTime x && isH x = (show t)
	| all isNumber x && t<10 = zero ++ (show t)
	| all isNumber x = (show t)
	| otherwise = []


changeMinutes :: [Char] -> Int -> [Char]
changeMinutes x t
	| isTime x && isHMS x && t<10 = getHour x ++ colon ++ zero ++ (show t) ++ colon ++ getSeconds x
	| isTime x && isHMS x = getHour x ++ colon ++ (show t) ++ colon ++ getSeconds x
	| isTime x && isHM x && t<10 = getHour x ++ colon ++ zero ++ (show t)
	| isTime x && isHM x = getHour x ++ colon ++ (show t)
	| otherwise = []

changeSeconds :: [Char] -> Int -> [Char]
changeSeconds x t
	| isTime x && isHMS x && t<10 = getHour x ++ colon ++ getMinutes x ++ colon ++ zero ++ (show t)
	| isTime x && isHMS x = getHour x ++ colon ++ getMinutes x  ++ colon ++ (show t)
	| otherwise = []

deleteLastTimePosition :: [Char] -> [Char]
deleteLastTimePosition x
	| isTime x && isHMS x = getHour x ++ colon ++ getMinutes x
	| isTime x && isHM x = getHour x
	| otherwise = []

increaseHour :: [Char] -> Int -> [Char]
increaseHour x t
	| isTime x && isHMS x && m<10 = zero ++ show m ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHMS x = show m ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHM x && m<10 = zero ++ show m ++ colon ++ getMinutes x
	| isTime x && isHM x = show m ++ colon ++ getMinutes x
	| isTime x && isH x && m<10 = zero ++ show m
	| isTime x && isH x = show m
	| otherwise = []
	where m = ((read $ getHour x :: Int)+t) `mod` 24

decreaseHour :: [Char] -> Int -> [Char]
decreaseHour x t
	| isTime x && isHMS x && m<10 = zero ++ show m ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHMS x = show m ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && isHM x && m<10 = zero ++ show m ++ colon ++ getMinutes x
	| isTime x && isHM x = show m ++ colon ++ getMinutes x
	| isTime x && isH x && m<10 = zero ++ show m
	| isTime x && isH x = show m
	| otherwise = []
	where m = ((read $ getHour x :: Int)-t) `mod` 24

increaseMinutes :: [Char] -> Int -> [Char]
increaseMinutes x t
	| isTime x && isHMS x && m<10 = getHour x ++ colon ++ zero ++ show m ++ colon ++ getSeconds x
	| isTime x && isHMS x = getHour x ++ colon ++ show m ++ colon ++ getSeconds x
	| isTime x && isHM x && m<10 = getHour x ++ colon ++ zero ++ show m
	| isTime x && isHM x = getHour x ++ colon ++ show m
	| otherwise = []
	where m = ((read $ getMinutes x :: Int)+t) `mod` 60
	
decreaseMinutes :: [Char] -> Int -> [Char]
decreaseMinutes x t
	| isTime x && isHMS x && m<10 = getHour x ++ colon ++ zero ++ show m ++ colon ++ getSeconds x
	| isTime x && isHMS x = getHour x ++ colon ++ show m ++ colon ++ getSeconds x
	| isTime x && isHM x && m<10 = getHour x ++ colon ++ zero ++ show m
	| isTime x && isHM x = getHour x ++ colon ++ show m
	| otherwise = []
	where m = ((read $ getMinutes x :: Int)-t) `mod` 60
	
increaseSeconds :: [Char] -> Int -> [Char]
increaseSeconds x t
	| isTime x && isHMS x && m<10 = getHour x ++ colon ++ getMinutes x ++ colon ++ zero ++ show m
	| isTime x && isHMS x = getHour x ++ colon ++ getMinutes x ++ colon ++ zero ++ show m
	| otherwise = []
	where m = ((read $ getSeconds x :: Int)+t) `mod` 60
	
decreaseSeconds :: [Char] -> Int -> [Char]
decreaseSeconds x t
	| isTime x && isHMS x && m<10 = getHour x ++ colon ++ getMinutes x ++ colon ++ zero ++ show m
	| isTime x && isHMS x = getHour x ++ colon ++ getMinutes x ++ colon ++ show m
	| otherwise = []
	where m = ((read $ getSeconds x :: Int)-t) `mod` 60


convertTimeTo24hoursFormat :: [Char] -> [Char]
convertTimeTo24hoursFormat x
	| isTime x && containsHMS x && isPM x && h==12 = "00" ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && containsHMS x && isPM x && h>0 && h<12 = show (h+12) ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && containsHMS x && isAM x && h<10 = zero ++ getHour x ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && containsHMS x && isAM x = getHour x ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && containsHMS x = getHour x ++ colon ++ getMinutes x ++ colon ++ getSeconds x
	| isTime x && containsHM x && isPM x && h==12 = "00" ++ colon ++ getMinutes x
	| isTime x && containsHM x && isPM x && h>0 && h<12 = show (h+12) ++ colon ++ getMinutes x
	| isTime x && containsHM x && isAM x && h<10 = zero ++ getHour x ++ colon ++ getMinutes x
	| isTime x && containsHM x && isAM x = getHour x ++ colon ++ getMinutes x
	| isTime x && containsHM x = getHour x ++ colon ++ getMinutes x
	| isTime x && isH x && isPM x && h==12 = "00" ++ oclock
	| isTime x && isH x && isPM x && h>0 && h<12 = show (h+12) ++ oclock
	| isTime x && isH x && isAM x && h<10 = zero ++ getHour x ++ oclock
	| isTime x && isH x && isAM x = getHour x ++ oclock
	| isTime x && isH x = getHour x ++ oclock
	| otherwise = []
	where h = read $ getHour x :: Int


convertTimeTo12hoursFormat :: [Char] -> [Char]
convertTimeTo12hoursFormat x
	| isTime x && containsHMS x && h==0 = "12" ++ colon ++ getMinutes x ++ colon ++ getSeconds x ++ " AM"
	| isTime x && containsHMS x && h==12 = "12" ++ colon ++ getMinutes x ++ colon ++ getSeconds x ++ " PM"
	| isTime x && containsHMS x && h>0 && h<10 = zero ++ show h ++ colon ++ getMinutes x ++ colon ++ getSeconds x ++ " AM"
	| isTime x && containsHMS x && h>9 && h<12 = show h ++ colon ++ getMinutes x ++ colon ++ getSeconds x ++ " AM"
	| isTime x && containsHMS x && h<22 = zero ++ show (h-12) ++ colon ++ getMinutes x ++ colon ++ getSeconds x	 ++ " PM"
	| isTime x && containsHMS x && h<24 = show (h-12) ++ colon ++ getMinutes x ++ colon ++ getSeconds x	 ++ " PM"
	| isTime x && containsHM x && h==0 = "12" ++ colon ++ getMinutes x ++ " AM"
	| isTime x && containsHM x && h==12 = "12" ++ colon ++ getMinutes x ++ " PM"
	| isTime x && containsHM x && h>0 && h<10 = zero ++ show h ++ colon ++ getMinutes x ++ " AM"
	| isTime x && containsHM x && h>9 && h<12 = show h ++ colon ++ getMinutes x ++ " AM"
	| isTime x && containsHM x && h<22 = zero ++ show (h-12) ++ colon ++ getMinutes x ++ " PM"
	| isTime x && containsHM x && h<24 = show (h-12) ++ colon ++ getMinutes x ++ " PM"
	| isTime x && isH x && h==0 = "12" ++ " AM"
	| isTime x && isH x && h==12 = "12" ++ " PM"
	| isTime x && isH x && h>0 && h<10 = zero ++ show h ++ " AM"
	| isTime x && isH x && h>9 && h<12 = show h ++ " AM"
	| isTime x && isH x && h<22 = zero ++ show (h-12) ++ " PM"
	| isTime x && isH x && h<24 = show (h-12) ++ " PM"
	| otherwise = []
	where h = read $ getHour x :: Int


convertTimeFormat :: [Char] -> [Char] -> [Char]
convertTimeFormat x f
	| f == "12" || f == "12h" = convertTimeTo12hoursFormat x
	|(toUpperString f) == "PM" || (toUpperString f) == "AM" = convertTimeTo12hoursFormat x ++ space ++ f
	| f == "24" || f == "24h" = convertTimeTo24hoursFormat x
	| otherwise = []

get12hoursFormatAuxiliar :: [Char] -> [Char]
get12hoursFormatAuxiliar x 
	| isPM x = "PM"
	| isAM x = "AM"
	| otherwise = []

delete12hoursFormatAuxiliar :: [Char] -> [Char]
delete12hoursFormatAuxiliar x = removeWords x ["AM","PM","am","pm"]

removeWords :: [Char] -> [[Char]] -> [Char]
removeWords l w = init . concatMap (++" ") . filter (not . (`elem` w)) . splitOn " " $ l

--change12hoursFormatAuxiliar

integerToTime :: Int -> [Char]
integerToTime x 
	| x>=0 && x<24 = convertTimeTo24hoursFormat ((show x) ++ "h")
	| otherwise = []

appendTimeElement :: [Char] -> Int -> [Char]
appendTimeElement x i = x ++ space ++ (timesList!!i)
appendTimeElement _ _ = []

convertTimeByTimeZone :: [Char] -> [Char] -> [Char] -> [Char]
convertTimeByTimeZone x zi zo
	| isTime x && Map.member zi timeZones && Map.member zo timeZones && (zi=="UTC" || zi=="GMT" || zi=="WET") = increaseHour x (fromIntegral (fromJust (Map.lookup zo timeZones)))	
	| isTime x && Map.member zi timeZones && Map.member zo timeZones && (zo=="UTC" || zo=="GMT" || zo=="WET") = decreaseHour x (fromIntegral (fromJust (Map.lookup zi timeZones)))
	| isTime x && Map.member zi timeZones && Map.member zo timeZones = increaseHour (decreaseHour x (fromIntegral (fromJust (Map.lookup zi timeZones)))) (fromIntegral (fromJust (Map.lookup zo timeZones)))
	| otherwise = []




--para strings
insert_first :: [[Char]] -> [Char] -> [[Char]]
insert_first x a = [(filter (not . (`elem` "'")) a)]++x
insert_first _ _ = []

insert_last :: [[Char]] -> [Char] -> [[Char]]
insert_last x a = x ++ [(filter (not . (`elem` "'")) a)]
insert_last _ _ = []

deleteParentheses :: [Char] -> [Char]
deleteParentheses xs = [ x | x <- xs, not (x `elem` "()") ]
deleteParentheses _ = []


-- changeSomePunctuationString a x y
changeSomePunctuationString :: [Char] -> [Char] -> [Char] -> [Char]
changeSomePunctuationString (x:xs) y z
	| [x] == y = z ++ changeSomePunctuationString xs y z
	| otherwise = [x] ++ changeSomePunctuationString xs y z
changeSomePunctuationString _ _ _ = []


-- para phones

addPhonePrefix :: [Char] -> Int -> [Char]
addPhonePrefix x p = lparentheses ++ (show p) ++ rparentheses ++ space ++ x

addPhonePrefixByCountry :: [Char] -> [Char] -> [Char]
addPhonePrefixByCountry x c = addPhonePrefix x (read $ fromJust (Map.lookup (fromJust (Map.lookup c countryCodes)) countryPhoneCodes) :: Int)

addPhonePrefixByCountryCode :: [Char] -> [Char] -> [Char]
addPhonePrefixByCountryCode x c = addPhonePrefix x (read $ fromJust (Map.lookup c countryPhoneCodes) :: Int)

addPlusInPrefix :: [Char] -> [Char]
addPlusInPrefix x
	| getFirstCharacter x == lparentheses = appendPositionString x plus 0
	| otherwise = []

getPhoneNumber :: [Char] -> [Char]
getPhoneNumber x 
	| (x =~ "\\d{3}[-]\\d{3}[-]\\d{4}" :: Bool) == True = (x =~ "\\d{3}[-]\\d{3}[-]\\d{4}" :: [Char])
	| (x =~ "\\d{2}[-]\\d{3}[-]\\d{2}[-]\\d{2}" :: Bool) == True = (x =~ "\\d{2}[-]\\d{3}[-]\\d{2}[-]\\d{2}" :: [Char])
	| otherwise = []


unitsConversion :: Float -> [Char] -> [Char] -> Float
unitsConversion x u1 u2
	--- Length
	| (Map.member u1 lengthScaleList) && (Map.member u2 lengthScaleList) && (u1_l==u2_l) = x
	| (Map.member u1 lengthScaleList) && (Map.member u2 lengthScaleList)= x * (10^^p_l)
	--- Mass
	| (Map.member u1 massScaleList) && (Map.member u2 massScaleList) && (u1_m==u2_m) = x
	| (Map.member u1 massScaleList) && (Map.member u2 massScaleList)= x * (10^^p_m)
	--- Time
	| (Map.member u1 timeScaleList) && (Map.member u2 timeScaleList) && (u1_t==u2_t) = x
	| (Map.member u1 timeScaleList) && (Map.member u2 timeScaleList)= x * (10^^p_t)
	--- Electricity
	| (Map.member u1 electricityScaleList) && (Map.member u2 electricityScaleList) && (u1_l==u2_l) = x
	| (Map.member u1 electricityScaleList) && (Map.member u2 electricityScaleList)= x * (10^^p_l)
	--- Temperature
	| (Map.member u1 temperatureScaleList) && (Map.member u2 temperatureScaleList) && (u1_te==u2_te) = x
	| (Map.member u1 temperatureScaleList) && (Map.member u2 temperatureScaleList)= x * (10^^p_te)
	| otherwise = -1
	--- Volume
	| (Map.member u1 volumeScaleList) && (Map.member u2 volumeScaleList) && (u1_v==u2_v) = x
	| (Map.member u1 volumeScaleList) && (Map.member u2 volumeScaleList)= x * (10^^p_v)
	----
	| otherwise = -1
	where	
		u1_l=(fromJust (Map.lookup u1 lengthScaleList))
		u2_l=(fromJust (Map.lookup u2 lengthScaleList))
		p_l = u1_l-u2_l
		u1_m=(fromJust (Map.lookup u1 massScaleList))
		u2_m=(fromJust (Map.lookup u2 massScaleList))
		p_m = u1_m-u2_m
		u1_t=(fromJust (Map.lookup u1 timeScaleList))
		u2_t=(fromJust (Map.lookup u2 timeScaleList))
		p_t = u1_t-u2_t
		u1_e=(fromJust (Map.lookup u1 electricityScaleList))
		u2_e=(fromJust (Map.lookup u2 electricityScaleList))
		p_e = u1_e-u2_e
		u1_te=(fromJust (Map.lookup u1 temperatureScaleList))
		u2_te=(fromJust (Map.lookup u2 temperatureScaleList))
		p_te = u1_te-u2_te
		u1_v=(fromJust (Map.lookup u1 volumeScaleList))
		u2_v=(fromJust (Map.lookup u2 volumeScaleList))
		p_v = u1_v-u2_v

								
getUnits :: [Char] -> [Char] 
getUnits x
	| (x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool) = (x =~ "([A-z]{1,3})+$" :: [Char])
	| (x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool) = (x =~ "([A-z]{1,3})+$" :: [Char])
	| otherwise = []
	
getValue ::  [Char] -> Float	
getValue x	
	| (x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool) = read (x =~ "^\\d{1,}[.]*\\d{1,}" :: [Char]) :: Float
	| (x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool) = read (x =~ "^\\d{1,}" :: [Char]) :: Float
	| otherwise = -1
	
getSystem :: [Char] -> [Char] 
getSystem x
	| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) lengthScaleList) = "Length"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) lengthScaleList) = "Length"
	| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) massScaleList) = "Mass"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) massScaleList) = "Mass"
	| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) timeScaleList) = "Time"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) timeScaleList) = "Time"
	| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) electricityScaleList) = "Electricity"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) electricityScaleList) = "Electricity"
		| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) temperatureScaleList) = "Temperature"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) temperatureScaleList) = "Temperature"
	| ((x =~ "^\\d{1,}[.]*\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}[.]*\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) volumeScaleList) = "Volume"
	| ((x =~ "^\\d{1,}([ ][A-z]{1,3})+$" :: Bool) || (x =~ "^\\d{1,}([A-z]{1,3})+$" :: Bool)) && (Map.member (x =~ "([A-z]{1,3})+$" :: [Char]) volumeScaleList) = "Volume"
	| otherwise = []
	
setUnits :: Float -> [Char] -> [Char]  
setUnits x y = (show x) ++ " " ++ y
	
