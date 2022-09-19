#source functions
source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "15-vdem"

# https://www.v-dem.net
filename <-
  "./data/2020 sfr model data/vdem12.parquet"
vdem_raw <-
  arrow::read_parquet(filename)
vdem_indicators <-
  setDT(raw.data$log)[source == "V-DEM" &
                        include == 1] %>% pull(variablename)

# TO DO - bespoke
vdem_indicators <- c(vdem_indicators, "country_name", "year")

#subset relevant columns for analysis, and filter by year > 2000 to avoid old countries being included
vdem <- setDT(vdem_raw)[year >= 2000, 
                        ..vdem_indicators]
rm(vdem_raw)
vdem$iso3c <-
  country.code.name(vdem$country_name) #note that this does not produce a match for Zanzibar; that's fine, exclude it from the analysis.

# TO DO 
# Check for duplicate iso3c and year
vdem.n <- vdem[,.(.N),.(iso3c)][order(N)] #this only outputs PSE and SOM as being problematic - SSD is going to have less years because South Sudan
#became a country in 2011
#change from wide to long, and pick out relevant variables, and change variablename to character
setDT(vdem)
vdem <- melt(vdem, id.vars = c("iso3c", "year", "country_name"))
vdem <-
  vdem[complete.cases(iso3c), .(value = min(value)), 
       .(iso3c, 
         variablename = as.character(variable), 
         year)]
#take care of the issues with Palestine and Somalia here -
#take the minimum value as the tiebreaker (does this for all countries, but it only matters for Palestine and Somalia/Somaliland)
vdem <- na.omit(vdem) #one last omit check

vdem$iso3c_CONVERT <- country.code.name(vdem$iso3c)
check_convert_iso(vdem, iso3c, iso3c_CONVERT,SCRIPT) # this is different from other scripts

vdem$iso3c = vdem$iso3c_CONVERT
vdem = vdem %>% select(iso3c,year,value,variablename)

check_coverage(vdem,"iso3c", "year", SCRIPT)
check_duplicates(vdem,"iso3c", "year", "variablename",SCRIPT)
check_nas(vdem, SCRIPT) 

raw.data$vdem <- vdem
rmExcept("raw.data")