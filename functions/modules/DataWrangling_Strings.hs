module MagicHaskeller.DataWrangling_Strings where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S
import MagicHaskeller.DataWrangling_General
import MagicHaskeller.DataWrangling_Constants

-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------



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

-- take one element of an array 
takeTwoOfThreeArray :: [[Char]] -> Int -> Int -> [[Char]]
takeTwoOfThreeArray x y z = [x!!y] ++ [x!!z]

--- Juntar dos strings con un espacio o signo de puntuación (si hay dos, dos, si hay una una...  nada.)
joinStringsWithPunctuation :: [Char] -> [Char] -> [Char] -> [Char]
joinStringsWithPunctuation _ [] _ = []
joinStringsWithPunctuation [] _ _ = []
joinStringsWithPunctuation x y f = x ++ f ++ y

joinStringsWithoutPunctuation :: [Char] -> [Char] -> [Char]
joinStringsWithoutPunctuation _ [] = []
joinStringsWithoutPunctuation [] _ = []
joinStringsWithoutPunctuation x y = x ++ y

--- Añade un signo de puntuación aun array y genera string
joinArrayWithPunctuation :: [[Char]] -> [Char] -> [Char]
joinArrayWithPunctuation (x:xs) y 
	| xs==[] = concat([x] ++ setPunctuationArray xs y)
	| otherwise = concat([x] ++ [y] ++ setPunctuationArray xs y)
joinArrayWithPunctuation [] _ = []


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

setParentheses :: [Char] -> [Char]
setParentheses x = lparentheses ++ x ++ rparentheses

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