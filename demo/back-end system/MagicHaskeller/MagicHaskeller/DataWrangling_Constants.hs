module MagicHaskeller.DataWrangling_Constants where
import Language.Haskell.TH as TH

import Data.Array
import Text.Regex.PCRE
import qualified Data.Map as Map
import Data.Maybe

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

dotcom :: [Char]
dotcom = ".com"

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

