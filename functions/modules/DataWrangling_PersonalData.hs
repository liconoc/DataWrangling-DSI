module MagicHaskeller.DataWrangling_PersonalData where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import qualified Data.Text as T
import Data.List.Split as S
import MagicHaskeller.DataWrangling_Strings
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General

-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------
isNomenclature :: [Char] -> Bool
isNomenclature x 
	| elem x nomenclatureList == True = True
	| length (filter isAlpha (take 1 x)) == 1  && (take 1 (drop 1 x) == dot) = True
	| otherwise = False


----------------------------------------------------
--- Funciones específicas de datos personales  ------
----------------------------------------------------
addNomenclature :: [Char] -> Int -> [Char]
addNomenclature x y = nomenclatureList!!y ++ space ++ x

deleteNomenclature :: [Char] -> [Char]
deleteNomenclature x = unwords (filter (not . isNomenclature) (words x))

deleteNomenclatureAndPunctuation :: [Char] -> [Char]
deleteNomenclatureAndPunctuation x = deleteSomePunctuationString (deleteSomePunctuationString (deleteNomenclature x) comma) dot

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
	
reduceNameSecondWord :: [Char] -> [Char] -> [Char]
reduceNameSecondWord x y = x ++ comma ++ space ++ (toUpperString (take 1 y)) ++ dot