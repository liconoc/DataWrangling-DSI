-- 
-- (c) Susumu Katayama
--
{-# OPTIONS -XTemplateHaskell #-}
module MagicHaskeller.Individual(availableNames, prioritizedNamesToPg) where
import Language.Haskell.TH as TH
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Char(isDigit)
import Data.List(findIndex, findIndices, mapAccumL, mapAccumR)
import Data.Generics
import MagicHaskeller.LibTH
import MagicHaskeller.Types(size)
import MagicHaskeller.ProgGenSF(mkTrieOptSFIO)
import Prelude hiding (tail)
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General
import MagicHaskeller.DataWrangling_Dates
import MagicHaskeller.DataWrangling_Emails
import MagicHaskeller.DataWrangling_Names
import MagicHaskeller.DataWrangling_Words

-- | 'totals' is the set of available values (except partial functions) that can be included/excluded individually.
totals :: [Primitive]
totals = concat withDoubleRatio	
    --------------------------------
    -- Funciones de data wrangling--
    --------------------------------
	-- Números
	++ $(p [| 0 :: Int |] )
	++ $(p [| 1 :: Int |] )
	++ $(p [| 2 :: Int |] )
	++ $(p [| 3 :: Int |] )
	++ $(p [| 4 :: Int |] )
	++ $(p [| 5 :: Int |] )
	++ $(p [| 6 :: Int |] )
	++ $(p [| 7 :: Int |] )
	++ $(p [| 8 :: Int |] )
	++ $(p [| 9 :: Int |] )
	++ $(p [| 10 :: Int |] )
	++ $(p [| 11 :: Int |] )
	++ $(p [| 12 :: Int |] )
	++ $(p [| 13 :: Int |] )
	++ $(p [| 14 :: Int |] )
	++ $(p [| 15 :: Int |] )
	++ $(p [| 16 :: Int |] )
	++ $(p [| 17 :: Int |] )
	++ $(p [| 18 :: Int |] )
	++ $(p [| 19 :: Int |] )
	++ $(p [| 20 :: Int |] )
	++ $(p [| 21 :: Int |] )
	++ $(p [| 22 :: Int |] )
	++ $(p [| 23 :: Int |] )
	++ $(p [| 24 :: Int |] )
	++ $(p [| 25 :: Int |] )
	++ $(p [| 26 :: Int |] )
	++ $(p [| 27 :: Int |] )
	++ $(p [| 28 :: Int |] )
	++ $(p [| 29 :: Int |] )
	++ $(p [| 30 :: Int |] )
	++ $(p [| 31 :: Int |] )
	++ $(p [| 32 :: Int |] )
	++ $(p [| 33 :: Int |] )
	++ $(p [| 34 :: Int |] )
	++ $(p [| 35 :: Int |] )
	++ $(p [| 36 :: Int |] )
	++ $(p [| 37 :: Int |] )
	++ $(p [| 38 :: Int |] )
	++ $(p [| 39 :: Int |] )
	++ $(p [| 40 :: Int |] )
	++ $(p [| 41 :: Int |] )
	++ $(p [| 42 :: Int |] )
	++ $(p [| 43 :: Int |] )
	++ $(p [| 44 :: Int |] )
	++ $(p [| 45 :: Int |] )
	++ $(p [| 46 :: Int |] )
	++ $(p [| 47 :: Int |] )
	++ $(p [| 48 :: Int |] )
	++ $(p [| 49 :: Int |] )
	++ $(p [| 50 :: Int |] )
	++ $(p [| 51 :: Int |] )
	++ $(p [| 52 :: Int |] )
	++ $(p [| 53 :: Int |] )
	++ $(p [| 54 :: Int |] )
	++ $(p [| 55 :: Int |] )
	++ $(p [| 56 :: Int |] )
	++ $(p [| 57 :: Int |] )
	++ $(p [| 58 :: Int |] )
	++ $(p [| 59 :: Int |] )
	++ $(p [| 1900 :: Int |] )
	++ $(p [| 2000 :: Int |] )
    -- Constantes
	++ $(p [| dash :: [Char] |] )
	++ $(p [| slash :: [Char] |] )
	++ $(p [| dot :: [Char] |] )
	++ $(p [| comma :: [Char] |] )
	++ $(p [| colon  :: [Char] |] )
	++ $(p [| lBracket  :: [Char] |] )
	++ $(p [| rBracket  :: [Char] |] )
	++ $(p [| MagicHaskeller.DataWrangling_Constants.and :: [Char] |] )
	++ $(p [| andWithDots :: [Char] |] )
	++ $(p [| at :: [Char] |] )
	++ $(p [| hash :: [Char] |] )
	++ $(p [| lparentheses :: [Char] |] )
	++ $(p [| rparentheses :: [Char] |] )
	++ $(p [| space :: [Char] |] )
	++ $(p [| zero  :: [Char] |] )
	++ $(p [| nineteen  :: [Char] |] )
	++ $(p [| twenty  :: [Char] |] )
	++ $(p [| firstElement :: [Char] |] )
	++ $(p [| middleElement :: [Char] |] )
	++ $(p [| lastElement :: [Char] |] )
	++ $(p [| dmy :: [Char] |] )
	++ $(p [| dym :: [Char] |] )
	++ $(p [| mdy :: [Char] |] )
	++ $(p [| myd :: [Char] |] )
	++ $(p [| ymd :: [Char] |] )
	++ $(p [| ydm :: [Char] |] )
	++ $(p [| dm :: [Char] |] )
	++ $(p [| my :: [Char] |] )
	++ $(p [| dy :: [Char] |] )
	++ $(p [| md :: [Char] |] )
	++ $(p [| ym :: [Char] |] )
	++ $(p [| yd :: [Char] |] )
	++ $(p [| dotcom :: [Char] |] )
	-- General
	--
	-- Fechas
	++ $(p [| getDayCardinalString :: [Char] -> [Char] |] )
	++ $(p [| getDayCardinalArray :: [[Char]] -> [Char] |] )
	++ $(p [| getDayOrdinal :: [Char] -> [Char] |] )
	++ $(p [| getWeekDayArray :: [[Char]] -> [Char] |] )
	++ $(p [| getWeekDayString :: [Char] -> [Char] |] )
	++ $(p [| getMonthNameString :: [Char] -> [Char] |] )
	++ $(p [| getMonthNameArray :: [[Char]] -> [Char] |] )
	++ $(p [| convertMonth :: [Char] -> [Char] |] )
	++ $(p [| getYearString :: [Char] -> [Char] |] )
	++ $(p [| getYearArray :: [[Char]] -> [Char] |] )
	++ $(p [| convertMonthToNumeric :: [Char] -> [Char] |] )
	++ $(p [| convertMonthToString :: [Char] -> [Char] |] )
	++ $(p [| takeTwoOfThreeArray :: [[Char]] -> Int -> Int -> [[Char]] |] )
	++ $(p [| getMonthArray :: [[Char]] -> [Char] |] )
	++ $(p [| getMonthString :: [Char] -> [Char] |] )
	++ $(p [| convertMonthToNumericWithinArray :: [[Char]] -> [[Char]] |] )
	++ $(p [| convertMonthToStringWithinArray :: [[Char]] -> [[Char]] |] )
	++ $(p [| reduceMonthWithinArray :: [[Char]] -> [[Char]] |] )	
	++ $(p [| changeDateFormat :: [Char] -> [Char] -> [[Char]] |] )
	++ $(p [| convertDayOrdinalWithinArray :: [[Char]] -> [[Char]] |] )
	++ $(p [| reduceYear :: [Char] -> [Char] |] )
	++ $(p [| reduceYearWithinArray :: [[Char]] -> [[Char]] |] )
	++ $(p [| reduceMonth :: [Char] -> [Char] |] )
	-- Emails
	++ $(p [| getWordsBeforeAt :: [Char] -> [Char] |] )
	++ $(p [| getWordsAfterAt :: [Char] -> [Char] |] )
	++ $(p [| getWordsBeforeDot :: [Char] -> [Char] |] )
	++ $(p [| getWordsAfterDot :: [Char] -> [Char] |] )
	++ $(p [| getWordsBetweenAtAndDot :: [Char] -> [Char] |] )
	++ $(p [| appendAt :: [Char] -> [Char] |] )
	++ $(p [| prependAt :: [Char] -> [Char] |] )
	++ $(p [| joinStringsWithAt :: [Char] -> [Char] -> [Char] |] )
	-- Names
	++ $(p [| addMaleNomenclature :: [Char] -> Int -> [Char] |] )
	++ $(p [| addFemaleNomenclature :: [Char] -> Int -> [Char] |] )
	++ $(p [| deleteNomenclature :: [Char] -> [Char] |] )
	++ $(p [| getNomenclature :: [Char] -> [Char] |] )
	++ $(p [| reduceNameSecondWord :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| getGenderByNomenclature :: [Char] -> [Char] |] )
	++ $(p [| deleteNomenclatureAndPunctuation :: [Char] -> [Char] |] )
	++ $(p [| reduceNamesFirstPlace :: [Char] -> [Char] |] )
	++ $(p [| reduceNameFirstPlace :: [Char] -> [Char] |] )
	++ $(p [| reduceNameWithSurnameSecondPlace :: [Char] -> [Char] |] )
	++ $(p [| reduceNameWithSurnamesSecondPlace :: [Char] -> [Char] |] )
	++ $(p [| initialsNameFirstPlace :: [Char] -> [Char] |] )
	-- Words
	++ $(p [| addPunctuationString :: [Char] -> [Char] -> [[Char]] |] )
	++ $(p [| splitStringWithoutPunctuation :: [Char] -> [[Char]] |] )
	++ $(p [| setPunctuationArray :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| changePunctuationArray :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| changePunctuationString :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| deletePunctuationArray :: [[Char]] -> [[Char]] |] )
	++ $(p [| deletePunctuationString :: [Char] -> [Char] |] )
	++ $(p [| deleteSomePunctuationString :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| splitStringByPunctuation :: [Char] -> [Char] -> [[Char]] |] )
	++ $(p [| splitStringWithPunctuation :: [Char] -> [[Char]] |] )
	++ $(p [| splitStringTakeOffPunctuation :: [Char] -> [[Char]] |] )
	++ $(p [| swapElementsString :: Int -> Int -> [Char] -> [Char] |] )
	++ $(p [| swapElementsArray :: Int -> Int -> [[Char]] -> [[Char]] |] )
	++ $(p [| appendPositionArray :: [[Char]] -> [Char] -> Int -> [[Char]] |] )
	++ $(p [| appendPositionString :: [Char] -> [Char] -> Int -> [Char] |] )
	++ $(p [| appendNextToLast :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| append :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| append_first :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| append_middle :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| append_last :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| prepend :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| prepend_first :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| prepend_middle :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| prepend_last :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| replacePositionArray :: [[Char]] -> [Char] -> Int -> [[Char]] |] )
	++ $(p [| replacePositionString :: [Char] -> [Char] -> Int -> [Char] |] )
	++ $(p [| replacePositionArrayFixedSize :: [[Char]] -> [Char] -> [Char] -> [[Char]] |] )
	++ $(p [| replaceAll :: [Char] -> [Char] -> [Char] -> [Char] |] )
	++ $(p [| replaceNextToLast :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| toLowString :: [Char] -> [Char] |] )
	++ $(p [| toUpperString :: [Char] -> [Char] |] )
	++ $(p [| reduceWord :: [Char] -> Int -> [Char] |] )
	++ $(p [| takeOneOfArray :: [[Char]] -> Int -> [Char] |] )
	++ $(p [| takeOneOfFixedSizeArray :: [[Char]] -> [Char] -> [Char] |] )
	++ $(p [| takeOneOfFixedSizeString :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| joinStringsWithPunctuation :: [Char] -> [Char] -> [Char] -> [Char] |] )
	++ $(p [| getOneWordByPosition :: [Char] -> Int -> [Char] |] )
	++ $(p [| getFirstWord :: [Char] -> [Char] |] )
	++ $(p [| getLastWord :: [Char] -> [Char] |] )
	++ $(p [| getOneCharacterByPosition :: [Char] -> Int -> [Char] |] )
	++ $(p [| getFirstCharacter :: [Char] -> [Char] |] )
	++ $(p [| getLastCharacter :: [Char] -> [Char] |] )
	++ $(p [| getStartToFirstSymbolOccurrence :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| getStartToLastSymbolOccurrence :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| getLastSymbolOccurrenceToEnd :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| getFirstSymbolOccurrenceToEnd :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| joinArrayWithPunctuation :: [[Char]] -> [Char] -> [Char] |] )
	++ $(p [| joinStringsWithoutPunctuation :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| setParentheses :: [Char] -> [Char] |] )
	++ $(p [| getCaps :: [Char] -> [Char] |] )
	++ $(p [| reduceSpaces :: [Char] -> [Char] |] )
	++ $(p [| setBrackets :: [Char] -> [Char] |] )
	++ $(p [| completeBrackets :: [Char] -> [Char] |] )
	++ $(p [| completeParentheses :: [Char] -> [Char] |] )
	++ $(p [| getFirstDigitToEnd :: [Char] -> [Char] |] )
	++ $(p [| getStartToFirstDigit :: [Char] -> [Char] |] )
	++ $(p [| getTime :: [Char] -> [Char] |] )
	++ $(p [| getHour :: [Char] -> [Char] |] )
	++ $(p [| getMinutes :: [Char] -> [Char] |] )
	++ $(p [| getSeconds :: [Char] -> [Char] |] )
	++ $(p [| appendOclockTime :: [Char] -> [Char] |] )
	++ $(p [| appendSomeTime :: [Char] -> Int -> [Char] |] )
	++ $(p [| changeHour :: [Char] -> Int -> [Char] |] )
	++ $(p [| changeMinutes :: [Char] -> Int -> [Char] |] )
	++ $(p [| changeSeconds :: [Char] -> Int -> [Char] |] )
	++ $(p [| deleteLastTimePosition :: [Char] -> [Char] |] )
	++ $(p [| increaseHour :: [Char] -> Int -> [Char] |] )
	++ $(p [| decreaseHour :: [Char] -> Int -> [Char] |] )
	++ $(p [| increaseMinutes :: [Char] -> Int -> [Char] |] )
	++ $(p [| decreaseMinutes :: [Char] -> Int -> [Char] |] )
	++ $(p [| increaseSeconds :: [Char] -> Int -> [Char] |] )
	++ $(p [| decreaseSeconds :: [Char] -> Int -> [Char] |] )
	++ $(p [| convertTimeTo24hoursFormat :: [Char] -> [Char] |] )
	++ $(p [| convertTimeTo12hoursFormat :: [Char] -> [Char] |] )
	++ $(p [| convertTimeFormat :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| get12hoursFormatAuxiliar :: [Char] -> [Char] |] )
	++ $(p [| delete12hoursFormatAuxiliar :: [Char] -> [Char] |] )
	++ $(p [| removeWords :: [Char] -> [[Char]] -> [Char] |] )
	++ $(p [| integerToTime :: Int -> [Char] |] )
	++ $(p [| appendTimeElement :: [Char] -> Int -> [Char] |] )
	++ $(p [| convertTimeByTimeZone :: [Char] -> [Char] -> [Char] -> [Char] |] )
	++ $(p [| insert_first :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| insert_last :: [[Char]] -> [Char] -> [[Char]] |] )
	++ $(p [| deleteParentheses :: [Char] -> [Char] |] )
	++ $(p [| changeSomePunctuationString :: [Char] -> [Char] -> [Char] -> [Char] |] )
	++ $(p [| getName :: [Char] -> [Char] |] )
	++ $(p [| getSurname :: [Char] -> [Char] |] )
	++ $(p [| addPhonePrefix :: [Char] -> Int -> [Char] |] )
	++ $(p [| addPhonePrefixByCountry :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| addPhonePrefixByCountryCode :: [Char] -> [Char] -> [Char] |] )
	++ $(p [| addPlusInPrefix :: [Char] -> [Char] |] )
	++ $(p [| getPhoneNumber :: [Char] -> [Char] |] )
	++ $(p [| loginString :: [Char] -> [Char] |] )
	++ $(p [| loginByNameString :: [Char] -> [Char] |] )
	++ $(p [| unitsConversion :: Float -> [Char] -> [Char] -> Float |] )
	++ $(p [| getUnits :: [Char] -> [Char]  |] )
	++ $(p [| getSystem :: [Char] -> [Char]	 |] )
	++ $(p [| setUnits :: Float -> [Char] -> [Char] |] )


-- You can add functions you like here, e.g. 
-- totals = concat withDoubleRatio ++ $(p [| nat_para :: (->) Int (a -> (Int -> a -> a) -> a) |] )

-- | 'partials' is the set of available partial functions that can be included/excluded individually.
partials :: [(Primitive,Primitive)]
partials = concat tupartialssNormal 

-- | 'aliases' is the set of aliases that can be used instead of the exact names appearing in 'totals' in order to increase readability of primitive library files. Also, aliases can be used to group a set of primitives and enable at once.
aliases :: [(String, [Primitive])]
aliases = [ ("total init", $(p [| reverse . drop 1 . reverse :: [a] -> [a] |])),
            ("total head", $(p [| foldr const :: a -> (->) [a] a |])),
            ("total last", $(p [| last' :: a -> [a] -> a |])),
            ("drop 1",     $(p [| tail :: (->) [a] [a] |] )),
            ("foldl",      $(p [| flip . flip foldl :: a -> (->) [b] ((a -> b -> a) -> a) |])),
            ("foldr",      $(p [| flip . flip foldr :: a -> (->) [b] ((b -> a -> a) -> a) |])),
            ("maybe",      $(p [| flip . maybe :: a -> (->) (Maybe b) ((b -> a) -> a) |])),
            ("map",        $(p [| flip map :: (->) ([a]) ((a -> b) -> [b]) |])),
            ("concatMap",  $(p [| flip concatMap :: (->) ([a]) ((a -> [b]) -> [b]) |])),
            ("any",        $(p [| flip any :: (->) ([a]) ((a -> Bool) -> Bool) |])),
            ("all",        $(p [| flip all :: (->) ([a]) ((a -> Bool) -> Bool) |])),
            ("zipWith",    $(p [| flip . flip zipWith :: (->) ([a]) ((->) ([b]) ((a -> b -> c) -> [c])) |])),
            ("either",     $(p [| flip (flip . either) :: (->) (Either a b) ((a -> c) -> (b -> c) -> c) |])),
            ("uncurry",    $(p [| flip uncurry :: (->) ((a, b)) ((a -> b -> c) -> c) |])),
            ("findIndex",  $(p [| flip findIndex :: (->) ([a]) ((a -> Bool) -> Maybe Int) |])),
            ("findIndices",$(p [| flip findIndices :: (->) ([a]) ((a -> Bool) -> [Int]) |])),
            ("mapAccumL",  $(p [| flip . flip mapAccumL :: acc -> (->) ([x]) ((acc -> x -> (acc, y)) -> (acc, [y])) |])),
            ("mapAccumR",  $(p [| flip . flip mapAccumR :: acc -> (->) ([x]) ((acc -> x -> (acc, y)) -> (acc, [y])) |])),
            ("\\n x f -> iterate f x !! (n::Int)", $(p [| nat_cata :: (->) Int (a -> (a -> a) -> a) |])),
            ("\\n x f -> iterate f x !! (n::Integer)", $(p [| nat_cata :: (->) Integer (a -> (a -> a) -> a) |]))
          ]

-- `normalizeSpaces' removes redundant spaces.
normalizeSpaces = unwords . words

mapAvailables :: M.Map String (Either [Primitive] (Primitive,Primitive))
mapAvailables = M.fromList assocAvailables
-- When dumping the available names, 'assocAvailables' is used instead of mapAvailables because I guess they should not be sorted
assocAvailables = [ (normalizeSpaces s, Left prims) | (s, prims) <- aliases ] ++ [ (pprintPrim prim, Left [prim]) | prim <- totals ] ++ [ (pprintPrim prim, Right tup) | tup@(_,prim) <- partials ]

availableNames :: [String]
availableNames = map fst assocAvailables

-- postprocessを使いたくなるけど，結果同じ表現になっちゃうとまずい．
pprintPrim :: Primitive -> String
pprintPrim (_, e@(VarE name), t) = 
  case nameBase name of 
    ('b':'y':d:'_':name) | isDigit d -> name                            -- Note that the type is omitted, because the class information is lost.
    ('-':'-':'#':name)           -> '(':dropWhile (=='#') name ++")"    -- Note that the type is omitted, because the class information is lost.
    _                                -> normalizeSpaces $ pprint $ TH.SigE (simplify e) t  -- normalizeSpaces is inserted just in case.
pprintPrim (_, e, t) = normalizeSpaces $ pprint $ TH.SigE (simplify e) t  -- normalizeSpaces is inserted just in case.

simplify :: TH.Exp -> TH.Exp
simplify = everywhere (mkT simp)
simp (ConE name) = ConE $ mkName $ nameBase name
simp (VarE name) = VarE $ mkName $ nameBase name
-- We should be careful about removing flips, because that will change the type.
-- simp (AppE (ConE name) e) | nameBase name == "flip" = e
-- simp (AppE (AppE (ConE name1) (ConE name2)) e) | (nameBase name1, nameBase name2) == ("flip",".") = e
simp e = e

namesToPrimitives :: [String] -> ([Primitive], [(Primitive,Primitive)])
namesToPrimitives xss = let ets = map ((mapAvailables !!!) . normalizeSpaces) xss
                        in ([ prim | Left prims <- ets, prim <- prims], [ tup | Right tup <- ets])


-- a !!! b = M.!
a !!! b = case M.lookup b a of Nothing -> error $ "!!! "++b
                               Just x -> x


namessToPrimitives :: [[String]] -> ([[Primitive]], [[(Primitive,Primitive)]])
namessToPrimitives nss = unzip $ map namesToPrimitives nss
prioritizedNamesToNamess :: [(Int,String)] -> [[String]]
prioritizedNamesToNamess ts = let mapPriorName = I.fromListWith (++) [(i,[s]) | (i,s) <- ts]
                              in map (\i -> maybe [] id $ I.lookup i mapPriorName) [fst $ I.findMin mapPriorName .. fst $ I.findMax mapPriorName]

prioritizedNamesToPg :: Maybe Int -> [(Int,String)] -> IO ProgGenSF
prioritizedNamesToPg Nothing   ts = pNTP options ts
prioritizedNamesToPg (Just sz) ts = pNTP options{memoCondPure = \t d -> size t < sz && 0<d {- && d<7 -}} ts

pNTP opt ts = mkPGXOpts mkTrieOptSFIO opt{tv1=True,nrands=repeat 20,timeout=Just 20000} (eqs++ords++doubleCls++ratioCls) clspartialss tot part
  where (tot,part) = namessToPrimitives $ prioritizedNamesToNamess ts

