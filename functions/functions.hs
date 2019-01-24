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


-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--------------------------- Comprobaciones (Bool) ---------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
isNomenclature :: [Char] -> Bool
isNomenclature x 
	| elem x nomenclatureMaleList == True || elem x nomenclatureFemaleList == True = True
--	| length (filter isAlpha (take 1 x)) == 1  && (take 1 (drop 1 x) == dot) = True
	| otherwise = False


-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--------------------------------- Constantes --------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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

underscore :: [Char]
underscore = "_"

plus :: [Char]
plus = "+"


--- Números 
--- Añadidos directamente en individual.hs

--- Números (formato char)

zero  :: [Char]
zero = "0"

nineteen  :: [Char]
nineteen = "19"

twenty  :: [Char]
twenty = "20"

oclock :: [Char]
oclock = ":00"

--- Posiciones array de formato/tamaño fijo
firstElement :: [Char]
firstElement = "firstElement"

middleElement :: [Char]
middleElement = "middleElement"

lastElement :: [Char]
lastElement = "lastElement"

--- Constantes de fechas

monthList :: [[Char]]
monthList = ["January",
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
		
monthListShort :: [[Char]]
monthListShort = ["Jan",
		"Feb",
		"Mar",
		"Apr",
		"May",
		"Jun",
		"Jul",
		"Aug",
		"Sep",
		"Oct",
		"Nov",
		"Dec"]
				
weekDayList :: [[Char]]
weekDayList = [	"Monday",
		"Tuesday",
		"Wednesday",
		"Thursday",
		"Friday",
		"Saturday",
		"Sunday"]
		
weekDayListShort :: [[Char]]
weekDayListShort = ["Mon",
		"Tue",
		"Wed",
		"Thu",
		"Fri",
		"Sat",
		"Sun"]

------------ 04/01/2018
		
timesList :: [[Char]]
timesList = ["UTC",
		"CST",
		"GMT",
		"PST",
		"EST",
		"MST",
		"WET",
		"HST",
		"AM",
		"PM",
		"am",
		"pm",
		"24",
		"24h",
		"h",
		"hr"]

	
dmy :: [Char]
dmy = "DMY"

dym :: [Char]
dym = "DYM"

mdy :: [Char]
mdy = "MDY"

myd :: [Char]
myd = "MYD"

ymd :: [Char]
ymd = "YMD"

ydm :: [Char]
ydm = "YDM"

dm :: [Char]
dm = "DM"

md :: [Char]
md = "MD"

dy :: [Char]
dy = "DY"

my :: [Char]
my = "MY"

ym :: [Char]
ym = "YM"

yd :: [Char]
yd = "YD"

d :: [Char]
d = "D"

m :: [Char]
m = "M"

y :: [Char]
y = "Y"

--- time

utc :: [Char]
utc = "UTC"

cst :: [Char]
cst = "CST"

pst :: [Char]
pst = "PST"

est :: [Char]
est = "EST"

gmt :: [Char]
gmt = "GMT"

mst :: [Char]
mst = "MST"

wet :: [Char]
wet = "WET"

cet :: [Char]
cet = "CET"

hst :: [Char]
hst = "HST"

hm :: [Char]
hm = "HH:MM"

hms :: [Char]
hms = "HH:MM:SS"

h :: [Char]
h = "HH"

hl :: [Char]
hl = "HHll"

--- constantes de names
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

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
------------------------------- Comprobaciones (Bool) -----------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
isDay :: [Char] -> Bool
isDay x 
	| 1 <= val && val <= 31 = True
	| otherwise = False
	where val = read x :: Int
	
isMonth :: [Char] -> Bool
isMonth x 
	| (elem x monthList == True || elem x monthListShort == True) = True
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
	

isNumeric :: [Char] -> Bool
isNumeric (x:xs) = (isDigit x) && (isNumeric xs)
isNumeric _ = True

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
	| (elem x weekDayList == True || elem x weekDayListShort) = True
	|otherwise = False

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


timeZones :: Map.Map [Char] Integer
timeZones = Map.fromList [	("UTC", 0),
							("GMT", 0),
							("WET", 0),
							("CET", 1),
							("EST", -5),
							("CST", -6),
							("MST", -7),
							("PST", -8),
							("HST", -10)
						]

	
	
--- 

countryCodes :: Map.Map [Char] [Char]
countryCodes = Map.fromList [	("Afghanistan","AFG"),
								("Albania","ALB"),
								("Algeria","DZA"),
								("American Samoa","ASM"),
								("Andorra","AND"),
								("Angola","AGO"),
								("Anguilla","AIA"),
								("Antarctica","ATA"),
								("Antigua and Barbuda","ATG"),
								("Argentina","ARG"),
								("Armenia","ARM"),
								("Aruba","ABW"),
								("Australia","AUS"),
								("Austria","AUT"),
								("Azerbaijan","AZE"),
								("Bahamas","BHS"),
								("Bahrain","BHR"),
								("Bangladesh","BGD"),
								("Barbados","BRB"),
								("Belarus","BLR"),
								("Belgium","BEL"),
								("Belize","BLZ"),
								("Benin","BEN"),
								("Bermuda","BMU"),
								("Bhutan","BTN"),
								("Bolivia","BOL"),
								("Bosnia and Herzegovina","BIH"),
								("Botswana","BWA"),
								("Brazil","BRA"),
								("British Indian Ocean Territory","IOT"),
								("British Virgin Islands","VGB"),
								("Brunei","BRN"),
								("Bulgaria","BGR"),
								("Burkina Faso","BFA"),
								("Burundi","BDI"),
								("Cambodia","KHM"),
								("Cameroon","CMR"),
								("Canada","CAN"),
								("Cape Verde","CPV"),
								("Cayman Islands","CYM"),
								("Central African Republic","CAF"),
								("Chad","TCD"),
								("Chile","CHL"),
								("China","CHN"),
								("Christmas Island","CXR"),
								("Cocos Islands","CCK"),
								("Colombia","COL"),
								("Comoros","COM"),
								("Cook Islands","COK"),
								("Costa Rica","CRI"),
								("Croatia","HRV"),
								("Cuba","CUB"),
								("Curacao","CUW"),
								("Cyprus","CYP"),
								("Czech Republic","CZE"),
								("Democratic Republic of the Congo","COD"),
								("Denmark","DNK"),
								("Djibouti","DJI"),
								("Dominica","DMA"),
								("Dominican Republic","DOM"),
								("East Timor","TLS"),
								("Ecuador","ECU"),
								("Egypt","EGY"),
								("El Salvador","SLV"),
								("Equatorial Guinea","GNQ"),
								("Eritrea","ERI"),
								("Estonia","EST"),
								("Ethiopia","ETH"),
								("Falkland Islands","FLK"),
								("Faroe Islands","FRO"),
								("Fiji","FJI"),
								("Finland","FIN"),
								("France","FRA"),
								("French Polynesia","PYF"),
								("Gabon","GAB"),
								("Gambia","GMB"),
								("Georgia","GEO"),
								("Germany","DEU"),
								("Ghana","GHA"),
								("Gibraltar","GIB"),
								("Greece","GRC"),
								("Greenland","GRL"),
								("Grenada","GRD"),
								("Guam","GUM"),
								("Guatemala","GTM"),
								("Guernsey","GGY"),
								("Guinea","GIN"),
								("Guinea-Bissau","GNB"),
								("Guyana","GUY"),
								("Haiti","HTI"),
								("Honduras","HND"),
								("Hong Kong","HKG"),
								("Hungary","HUN"),
								("Iceland","ISL"),
								("India","IND"),
								("Indonesia","IDN"),
								("Iran","IRN"),
								("Iraq","IRQ"),
								("Ireland","IRL"),
								("Isle of Man","IMN"),
								("Israel","ISR"),
								("Italy","ITA"),
								("Ivory Coast","CIV"),
								("Jamaica","JAM"),
								("Japan","JPN"),
								("Jersey","JEY"),
								("Jordan","JOR"),
								("Kazakhstan","KAZ"),
								("Kenya","KEN"),
								("Kiribati","KIR"),
								("Kosovo","XKX"),
								("Kuwait","KWT"),
								("Kyrgyzstan","KGZ"),
								("Laos","LAO"),
								("Latvia","LVA"),
								("Lebanon","LBN"),
								("Lesotho","LSO"),
								("Liberia","LBR"),
								("Libya","LBY"),
								("Liechtenstein","LIE"),
								("Lithuania","LTU"),
								("Luxembourg","LUX"),
								("Macau","MAC"),
								("Macedonia","MKD"),
								("Madagascar","MDG"),
								("Malawi","MWI"),
								("Malaysia","MYS"),
								("Maldives","MDV"),
								("Mali","MLI"),
								("Malta","MLT"),
								("Marshall Islands","MHL"),
								("Mauritania","MRT"),
								("Mauritius","MUS"),
								("Mayotte","MYT"),
								("Mexico","MEX"),
								("Micronesia","FSM"),
								("Moldova","MDA"),
								("Monaco","MCO"),
								("Mongolia","MNG"),
								("Montenegro","MNE"),
								("Montserrat","MSR"),
								("Morocco","MAR"),
								("Mozambique","MOZ"),
								("Myanmar","MMR"),
								("Namibia","NAM"),
								("Nauru","NRU"),
								("Nepal","NPL"),
								("Netherlands","NLD"),
								("Netherlands Antilles","ANT"),
								("New Caledonia","NCL"),
								("New Zealand","NZL"),
								("Nicaragua","NIC"),
								("Niger","NER"),
								("Nigeria","NGA"),
								("Niue","NIU"),
								("North Korea","PRK"),
								("Northern Mariana Islands","MNP"),
								("Norway","NOR"),
								("Oman","OMN"),
								("Pakistan","PAK"),
								("Palau","PLW"),
								("Palestine","PSE"),
								("Panama","PAN"),
								("Papua New Guinea","PNG"),
								("Paraguay","PRY"),
								("Peru","PER"),
								("Philippines","PHL"),
								("Pitcairn","PCN"),
								("Poland","POL"),
								("Portugal","PRT"),
								("Puerto Rico","PRI"),
								("Qatar","QAT"),
								("Republic of the Congo","COG"),
								("Reunion","REU"),
								("Romania","ROU"),
								("Russia","RUS"),
								("Rwanda","RWA"),
								("Saint Barthelemy","BLM"),
								("Saint Helena","SHN"),
								("Saint Kitts and Nevis","KNA"),
								("Saint Lucia","LCA"),
								("Saint Martin","MAF"),
								("Saint Pierre and Miquelon","SPM"),
								("Saint Vincent and the Grenadines","VCT"),
								("Samoa","WSM"),
								("San Marino","SMR"),
								("Sao Tome and Principe","STP"),
								("Saudi Arabia","SAU"),
								("Senegal","SEN"),
								("Serbia","SRB"),
								("Seychelles","SYC"),
								("Sierra Leone","SLE"),
								("Singapore","SGP"),
								("Sint Maarten","SXM"),
								("Slovakia","SVK"),
								("Slovenia","SVN"),
								("Solomon Islands","SLB"),
								("Somalia","SOM"),
								("South Africa","ZAF"),
								("South Korea","KOR"),
								("South Sudan","SSD"),
								("Spain","ESP"),
								("Sri Lanka","LKA"),
								("Sudan","SDN"),
								("Suriname","SUR"),
								("Svalbard and Jan Mayen","SJM"),
								("Swaziland","SWZ"),
								("Sweden","SWE"),
								("Switzerland","CHE"),
								("Syria","SYR"),
								("Taiwan","TWN"),
								("Tajikistan","TJK"),
								("Tanzania","TZA"),
								("Thailand","THA"),
								("Togo","TGO"),
								("Tokelau","TKL"),
								("Tonga","TON"),
								("Trinidad and Tobago","TTO"),
								("Tunisia","TUN"),
								("Turkey","TUR"),
								("Turkmenistan","TKM"),
								("Turks and Caicos Islands","TCA"),
								("Tuvalu","TUV"),
								("U.S. Virgin Islands","VIR"),
								("Uganda","UGA"),
								("Ukraine","UKR"),
								("United Arab Emirates","ARE"),
								("United Kingdom","GBR"),
								("United States","USA"),
								("Uruguay","URY"),
								("Uzbekistan","UZB"),
								("Vanuatu","VUT"),
								("Vatican","VAT"),
								("Venezuela","VEN"),
								("Vietnam","VNM"),
								("Wallis and Futuna","WLF"),
								("Western Sahara","ESH"),
								("Yemen","YEM"),
								("Zambia","ZMB"),
								("Zimbabwe","ZWE")
							]

countryPhoneCodes :: Map.Map [Char] [Char]
countryPhoneCodes = Map.fromList [	("AFG","93"),
									("ALB","355"),
									("ALB","355"),
									("DZA","213"),
									("ASM","1-684"),
									("AND","376"),
									("AGO","244"),
									("AIA","1-264"),
									("ATA","672"),
									("ATG","1-268"),
									("ARG","54"),
									("ARM","374"),
									("ABW","297"),
									("AUS","61"),
									("AUT","43"),
									("AZE","994"),
									("BHS","1-242"),
									("BHR","973"),
									("BGD","880"),
									("BRB","1-246"),
									("BLR","375"),
									("BEL","32"),
									("BLZ","501"),
									("BEN","229"),
									("BMU","1-441"),
									("BTN","975"),
									("BOL","591"),
									("BIH","387"),
									("BWA","267"),
									("BRA","55"),
									("IOT","246"),
									("VGB","1-284"),
									("BRN","673"),
									("BGR","359"),
									("BFA","226"),
									("BDI","257"),
									("KHM","855"),
									("CMR","237"),
									("CAN","1"),
									("CPV","238"),
									("CYM","1-345"),
									("CAF","236"),
									("TCD","235"),
									("CHL","56"),
									("CHN","86"),
									("CXR","61"),
									("CCK","61"),
									("COL","57"),
									("COM","269"),
									("COK","682"),
									("CRI","506"),
									("HRV","385"),
									("CUB","53"),
									("CUW","599"),
									("CYP","357"),
									("CZE","420"),
									("COD","243"),
									("DNK","45"),
									("DJI","253"),
									("DMA","1-767"),
									("DOM","1-809"),
									("TLS","670"),
									("ECU","593"),
									("EGY","20"),
									("SLV","503"),
									("GNQ","240"),
									("ERI","291"),
									("EST","372"),
									("ETH","251"),
									("FLK","500"),
									("FRO","298"),
									("FJI","679"),
									("FIN","358"),
									("FRA","33"),
									("PYF","689"),
									("GAB","241"),
									("GMB","220"),
									("GEO","995"),
									("DEU","49"),
									("GHA","233"),
									("GIB","350"),
									("GRC","30"),
									("GRL","299"),
									("GRD","1-473"),
									("GUM","1-671"),
									("GTM","502"),
									("GGY","44-1481"),
									("GIN","224"),
									("GNB","245"),
									("GUY","592"),
									("HTI","509"),
									("HND","504"),
									("HKG","852"),
									("HUN","36"),
									("ISL","354"),
									("IND","91"),
									("IDN","62"),
									("IRN","98"),
									("IRQ","964"),
									("IRL","353"),
									("IMN","44-1624"),
									("ISR","972"),
									("ITA","39"),
									("CIV","225"),
									("JAM","1-876"),
									("JPN","81"),
									("JEY","44-1534"),
									("JOR","962"),
									("KAZ","7"),
									("KEN","254"),
									("KIR","686"),
									("XKX","383"),
									("KWT","965"),
									("KGZ","996"),
									("LAO","856"),
									("LVA","371"),
									("LBN","961"),
									("LSO","266"),
									("LBR","231"),
									("LBY","218"),
									("LIE","423"),
									("LTU","370"),
									("LUX","352"),
									("MAC","853"),
									("MKD","389"),
									("MDG","261"),
									("MWI","265"),
									("MYS","60"),
									("MDV","960"),
									("MLI","223"),
									("MLT","356"),
									("MHL","692"),
									("MRT","222"),
									("MUS","230"),
									("MYT","262"),
									("MEX","52"),
									("FSM","691"),
									("MDA","373"),
									("MCO","377"),
									("MNG","976"),
									("MNE","382"),
									("MSR","1-664"),
									("MAR","212"),
									("MOZ","258"),
									("MMR","95"),
									("NAM","264"),
									("NRU","674"),
									("NPL","977"),
									("NLD","31"),
									("ANT","599"),
									("NCL","687"),
									("NZL","64"),
									("NIC","505"),
									("NER","227"),
									("NGA","234"),
									("NIU","683"),
									("PRK","850"),
									("MNP","1-670"),
									("NOR","47"),
									("OMN","968"),
									("PAK","92"),
									("PLW","680"),
									("PSE","970"),
									("PAN","507"),
									("PNG","675"),
									("PRY","595"),
									("PER","51"),
									("PHL","63"),
									("PCN","64"),
									("POL","48"),
									("PRT","351"),
									("PRI","1-787"),
									("QAT","974"),
									("COG","242"),
									("REU","262"),
									("ROU","40"),
									("RUS","7"),
									("RWA","250"),
									("BLM","590"),
									("SHN","290"),
									("KNA","1-869"),
									("LCA","1-758"),
									("MAF","590"),
									("SPM","508"),
									("VCT","1-784"),
									("WSM","685"),
									("SMR","378"),
									("STP","239"),
									("SAU","966"),
									("SEN","221"),
									("SRB","381"),
									("SYC","248"),
									("SLE","232"),
									("SGP","65"),
									("SXM","1-721"),
									("SVK","421"),
									("SVN","386"),
									("SLB","677"),
									("SOM","252"),
									("ZAF","27"),
									("KOR","82"),
									("SSD","211"),
									("ESP","34"),
									("LKA","94"),
									("SDN","249"),
									("SUR","597"),
									("SJM","47"),
									("SWZ","268"),
									("SWE","46"),
									("CHE","41"),
									("SYR","963"),
									("TWN","886"),
									("TJK","992"),
									("TZA","255"),
									("THA","66"),
									("TGO","228"),
									("TKL","690"),
									("TON","676"),
									("TTO","1-868"),
									("TUN","216"),
									("TUR","90"),
									("TKM","993"),
									("TCA","1-649"),
									("TUV","688"),
									("VIR","1-340"),
									("UGA","256"),
									("UKR","380"),
									("ARE","971"),
									("GBR","44"),
									("USA","1"),
									("URY","598"),
									("UZB","998"),
									("VUT","678"),
									("VAT","379"),
									("VEN","58"),
									("VNM","84"),
									("WLF","681"),
									("ESH","212"),
									("YEM","967"),
									("ZMB","260"),
									("ZWE","263")
							]
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--------------------- Funciones de manipulación de cadenas ------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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
	| length a == 9 = [take 2 a] ++ [take 3 (drop 2 a)] ++ [take 2 (drop 5 a)] ++ [take 2 (drop 7 a)]
	| length a == 10 = [take 3 a] ++ [take 3 (drop 3 a)] ++ [take 4 (drop 6 a)]
	| length a == 12 = [take 3 del] ++ [take 3 (drop 3 del)] ++ [take 4 (drop 6 del)]
	| otherwise = []
	where del=deletePunctuationString a


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

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
----------------- Funciones específicas de datos personales  ----------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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
	

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
---------------------- Funciones específicas de fechas ----------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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
	| day == "01" || day == "1" = "1" ++ "st"
	| day == "02" || day == "2" = "2" ++ "nd"
	| day == "03" || day == "3" = "3" ++ "rd"
	| otherwise = day ++ "th"
	where day = getDayCardinalString x	
	

getWeekDayArray :: [[Char]] -> [Char]
getWeekDayArray (x:xs)
	| (elem x weekDayList == True || elem x weekDayListShort) = x
	| otherwise = getWeekDayArray xs
getWeekDayArray _ = []

getWeekDayString :: [Char] -> [Char]
getWeekDayString x = getWeekDayArray (splitStringTakeOffPunctuation x)


-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
---- ACTUALIZADO 20/11/2017
---- NO TIENE ENCUENTA ABREVIATURAS
getMonthNameString :: [Char] -> [Char]
getMonthNameString x = getMonthNameArray (splitStringTakeOffPunctuation x)

getMonthNameArray :: [[Char]] -> [Char]
getMonthNameArray (x:xs)
	| (elem x monthList == True || elem x monthListShort == True) = x
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
--- 03/01/2018 si entra un string se muere: "Missing page numbers, 1993" .read no parse
getYearArray :: [[Char]] -> [Char]
getYearArray (x:xs)
	| isLongYear x = x
	| isShortYear x = x
	| otherwise = getYearArray xs
getYearArray _ = []
	
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

--- 03/01/2018 cómo narices va a funcionar si los formatos son strings???? hay que rehacerlo y hacer que los formatos sean constantes!!
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
	


--getDayOrdinal en un array.. no hay diferencia en fisrt, second, third. todo th
convertDayOrdinalWithinArray :: [[Char]] -> [[Char]]
convertDayOrdinalWithinArray  (x:xs)
	| isNumeric x && isDay x = [getDayOrdinal x] ++ xs
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

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
------------------------ Funciones específicas de correos  ------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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

dotcom :: [Char]
dotcom = ".com"

--- Juntar dos strings con arroba
joinStringsWithAt :: [Char] -> [Char] -> [Char]
joinStringsWithAt _ [] = []
joinStringsWithAt [] _ = []
joinStringsWithAt x y = x ++ at ++ y


-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
------------------------ Funciones específicas de tiempos  ------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

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


---- Logins de usuario (para names)
blendsList :: [[Char]]
blendsList = [	"bl",
				"br", 	
				"ch",	
				"cl",	
				"cr", 	
				"dr", 	
				"fl", 	
				"fr", 	
				"gl", 	
				"gr",	
				"pl", 	
				"pr", 	
				"sc",
				"sh", 	
				"sk", 	
				"sl", 	
				"sm", 	
				"sn", 	
				"sp", 	
				"st", 	
				"sw", 	
				"th", 	
				"tr", 	
				"tw", 	
				"wh", 	
				"wr",
				"sch", 	
				"scr", 	
				"shr", 	
				"sph", 	
				"spl", 	
				"spr", 	
				"squ", 	
				"str", 	
				"thr",
				-- español
				"ll",
				"rr"
			]

clusterVowelList :: [[Char]]
clusterVowelList = [	"Ai",
				"Ei", 	
				"Ii",	
				"Oi",	
				"Ui", 	
				"Ui", 	
				"Ia", 	
				"Ie", 	
				"Io", 	
				"Iu",	
				"Au", 	
				"Eu", 	
				"Iu",
				"Ou", 	
				"Uu", 	
				"Ua", 	
				"Ue", 	
				"Ui", 	
				"Uo", 	
				"Ahi"
			]

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

---------------		
----- Unidades de medida
----------------
nm_units :: [Char]
nm_units = "nm"
microm_units :: [Char]
microm_units = "microm"
mm_units :: [Char]
mm_units = "mm"
cm_units :: [Char]
cm_units = "cm"
dm_units :: [Char]
dm_units = "dm"
m_units :: [Char]
m_units = "m"
dam_units :: [Char]
dam_units = "dam"
hm_units :: [Char]
hm_units = "hm"
km_units :: [Char]
km_units = "km"
ng_units :: [Char]
ng_units = "ng"
microg_units :: [Char]
microg_units = "microg"
mg_units :: [Char]
mg_units = "mg"
cg_units :: [Char]
cg_units = "cg"
dg_units :: [Char]
dg_units = "dg"
g_units :: [Char]
g_units = "g"
dag_units :: [Char]
dag_units = "dag"
hg_units :: [Char]
hg_units = "hg"
kg_units :: [Char]
kg_units = "kg"
ns_units :: [Char]
ns_units = "ns"
micros_units :: [Char]
micros_units = "micros"
ms_units :: [Char]
ms_units = "ms"
cs_units :: [Char]
cs_units = "cs"
ds_units :: [Char]
ds_units = "ds"
s_units :: [Char]
s_units = "s"
das_units :: [Char]
das_units = "das"
hs_units :: [Char]
hs_units = "hs"
ks_units :: [Char]
ks_units = "ks"
nA_units :: [Char]
nA_units = "nA"
microA_units :: [Char]
microA_units = "microA"
mA_units :: [Char]
mA_units = "mA"
cA_units :: [Char]
cA_units = "cA"
dA_units :: [Char]
dA_units = "dA"
a_units :: [Char]
a_units = "A"
daA_units :: [Char]
daA_units = "daA"
hA_units :: [Char]
hA_units = "hA"
kA_units :: [Char]
kA_units = "kA"
nK_units :: [Char]
nK_units = "nK"
microK_units :: [Char]
microK_units = "microK"
mK_units :: [Char]
mK_units = "mK"
cK_units :: [Char]
cK_units = "cK"
dK_units :: [Char]
dK_units = "dK"
k_units :: [Char]
k_units = "K"
daK_units :: [Char]
daK_units = "daK"
hK_units :: [Char]
hK_units = "hK"
kK_units :: [Char]
kK_units = "kK"
nl_units :: [Char]
nl_units = "nl"
microl_units :: [Char]
microl_units = "microl"
ml_units :: [Char]
ml_units = "ml"
cl_units :: [Char]
cl_units = "cl"
dl_units :: [Char]
dl_units = "dl"
l_units :: [Char]
l_units = "l"
dal_units :: [Char]
dal_units = "dal"
hl_units :: [Char]
hl_units = "hl"
kl_units :: [Char]
kl_units = "kl"
								
---- Longitud 
lengthScaleList :: Map.Map [Char] Int
lengthScaleList = Map.fromList [
									("nm", 1),
									("microm", 2),
									("mm", 3),
									("cm", 4),
									("dm", 5),
									("m", 6),
									("dam", 7),
									("hm", 8),
									("km", 9)
								]
								

---- Masa
massScaleList :: Map.Map [Char] Int
massScaleList = Map.fromList [
									("ng", 1),
									("microg", 2),
									("mg", 3),
									("cg", 4),
									("dg", 5),
									("g", 6),
									("dag", 7),
									("hg", 8),
									("kg", 9)
								]
								
---- Tiempo
timeScaleList :: Map.Map [Char] Int
timeScaleList = Map.fromList [
									("ns", 1),
									("micros", 2),
									("ms", 3),
									("cs", 4),
									("ds", 5),
									("s", 6),
									("das", 7),
									("hs", 8),
									("ks", 9)
								]
--- caso especial, de minutos a cualquier otra cosa. primero pasar a segundos multiplicando por 60
							
---- Corriente eléctrica
electricityScaleList :: Map.Map [Char] Int
electricityScaleList = Map.fromList [
									("nA", 1),
									("microA", 2),
									("mA", 3),
									("cA", 4),
									("dA", 5),
									("A", 6),
									("daA", 7),
									("hA", 8),
									("kA", 9)
								]
							
---- Temperatura
temperatureScaleList :: Map.Map [Char] Int
temperatureScaleList = Map.fromList [
									("nK", 1),
									("microK", 2),
									("mK", 3),
									("cK", 4),
									("dK", 5),
									("K", 6),
									("daK", 7),
									("hK", 8),
									("kK", 9),
									("mK", 10)
								]
									
---- caso especial: celsius, farenheit

---------- DERIVADAS

--- Volumen (solo litro)
volumeScaleList :: Map.Map [Char] Int
volumeScaleList = Map.fromList  [
									("nl", 1),
									("microl", 2),
									("ml", 3),
									("cl", 4),
									("dl", 5),
									("l", 6),
									("dal", 7),
									("hl", 8),
									("kl", 9)
								]

---- casos especiales: pinta, onza, galón

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
	
