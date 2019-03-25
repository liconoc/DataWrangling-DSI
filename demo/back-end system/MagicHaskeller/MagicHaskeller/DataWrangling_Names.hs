module MagicHaskeller.DataWrangling_Names where
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
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General

-----------------------------
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

-- dos palabras	
reduceNameSecondWord :: [Char] -> [Char] -> [Char]
reduceNameSecondWord x y = x ++ comma ++ space ++ (toUpperString (take 1 y)) ++ dot

getGenderByNomenclature :: [Char] -> [Char]
getGenderByNomenclature x
	| elem nomenclature nomenclatureMaleList == True && elem nomenclature nomenclatureFemaleList == False = "Male"
	| elem nomenclature nomenclatureMaleList == False && elem nomenclature nomenclatureFemaleList == True = "Female"
	| elem nomenclature nomenclatureMaleList == True && elem nomenclature nomenclatureFemaleList == True = "Indeterminate"
	| otherwise = []
	where nomenclature = getNomenclature x

--------- NEW 25/05/2017 -----
deleteNomenclatureAndPunctuation :: [Char] -> [Char]
deleteNomenclatureAndPunctuation x = deleteSomePunctuationString (deleteSomePunctuationString (deleteNomenclature x) comma) dot

-- Dr. Eran Yahav -> E. Yahav (con uno o dos nombres)
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

-- Dr. Eran Yahav -> "Yahav, E."
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

-- Dr. Eran Yahav -> "E.Y."
initialsNameFirstPlace :: [Char] -> [Char]
initialsNameFirstPlace x = getFirstWord wrd ++ getFirstCharacter(getLastWord wrd) ++ dot
	where wrd = reduceNameFirstPlace x


-- para names
getName :: [Char] -> [Char]
getName x 
	| (x =~ "[A-Z][.]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = (getFirstWord xs) ++ space ++ (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][.]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getFirstWord xs) ++ space ++ (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = (getFirstWord xs) ++ space ++ (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getFirstWord xs) ++ space ++ (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = getFirstWord xs
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = getFirstWord xs
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = getFirstWord xs
	| (x =~ "[A-Z][a-z]+" :: Bool) == True = getFirstWord xs
	| otherwise = []
	where xs = deleteNomenclature x

-- getSurname
getSurname :: [Char] -> [Char]
getSurname x 
	| (x =~ "[A-Z][.]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 2)
	| (x =~ "[A-Z][.]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 2) ++ space ++ (getOneWordByPosition xs 3)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 2)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 2) ++ space ++ (getOneWordByPosition xs 3)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[-][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 1) ++ space ++ (getOneWordByPosition xs 2)
	| (x =~ "[A-Z][a-z]+[ ][A-Z][a-z]+" :: Bool) == True = (getOneWordByPosition xs 1)
	| (x =~ "[A-Z][a-z]+" :: Bool) == True = []
	| otherwise = []
	where xs = deleteNomenclature x



loginString :: [Char] -> [Char]
loginString x 
	-- 1.Two consonants between two vowels VCCV - split between them VC-CV as in cof-fee, pic-nic, except the "cluster consonant" that represents a single sound: meth-od, Ro-chester, hang-out
	| (x =~ "(^[AEIOU][^aeiou ,][^aeiou ,][aeiou])" :: Bool) == True && (elem (getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2) blendsList) = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2)
	| (x =~ "(^[AEIOU][^aeiou ,][^aeiou ,][aeiou])" :: Bool) == True = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1)
	-- 2.Three or more consonants between the vowels VCCCV - split keeping the blends together as in mon-ster or child-ren (this seems the most difficult as you cannot avoid a dictionary)
	| (x =~ "(^[AEIOU][^aeiou ,]{3,}[aeiou])" :: Bool) == True && (elem (getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2 ++ getOneCharacterByPosition x 3) blendsList) = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2 ++ getOneCharacterByPosition x 3)
	| (x =~ "(^[AEIOU][^aeiou ,]{3,}[aeiou])" :: Bool) == True && (elem (getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2) blendsList) = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1 ++ getOneCharacterByPosition x 2)
	| (x =~ "(^[AEIOU][^aeiou ,]{3,}[aeiou])" :: Bool) == True && (elem (getOneCharacterByPosition x 2 ++ getOneCharacterByPosition x 3) blendsList) = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1)
	-- 3.One consonant between two vowels VCV - split after the first vowel V-CV as in ba-con, a-rid
	-- The rule above also has an exception based on blends: cour-age, play-time
	| (x =~ "(^[AEIOU][^aeiou ,][aeiou])" :: Bool) == True = toLowString (getOneCharacterByPosition x 0)
	-- 4.Two vowels together VV - split between, except they represent a "cluster vowel": po-em, but glacier, earl-ier
	| (x =~ "(^[AEIOU][aeiou])" :: Bool) == True && (elem (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1) clusterVowelList) = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1)
	| (x =~ "(^[AEIOU][aeiou])" :: Bool) == True = toLowString (getOneCharacterByPosition x 0)
	| (x =~ "(^[A-z][.]*$)" :: Bool) == True = toLowString (getOneCharacterByPosition x 0)
	| (x =~ "(^[A-z])" :: Bool) == True = toLowString (getOneCharacterByPosition x 0 ++ getOneCharacterByPosition x 1)
	| otherwise = []
	
			
loginByNameString :: [Char] -> [Char]
loginByNameString x
	--- Si hay una coma, el nombre está después de ella entero o en inicial
	--- Dos apellidos
	| (x =~ "[,]" :: Bool) == True && length surnamesBeforeComma ==2  =  ( (loginString (reduceSpaces(splitByComma!!1))) ++(loginString (surnamesBeforeComma!!0)) ++ (loginString (surnamesBeforeComma!!1)) ) 
	--- Uno apellidos
	| (x =~ "[,]" :: Bool) == True && length surnamesBeforeComma ==1  =  ( (loginString (reduceSpaces(splitByComma!!1))) ++(loginString (surnamesBeforeComma!!0)) )
	--- Si no hay coma, asumo el nombre el primero
	--- Si tiene 4 words, asumo dos nombres y dos apellidos
	| length w == 4 = ( (loginString (w!!0)) ++ (loginString (w!!2)) ++ (loginString (w!!3)) )
	--- Si hay tres words, asumo dos apellidos
	| length w == 3 = ( (loginString (w!!0)) ++ (loginString (w!!1)) ++ (loginString (w!!2)) )
	--- si hay dos words, asumo un nombre y un apellido
	| length w == 2 = ( (loginString (w!!0)) ++ (loginString (w!!1)) )
	| otherwise = []
	where 	
		w = words x
		splitByComma = splitStringByPunctuation x comma
		surnamesBeforeComma = words (splitByComma!!0)




	

