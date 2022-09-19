source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

# https://www.wri.org/applications/aqueduct/water-risk-atlas/

SCRIPT = "19-water-stress"

dat_raw <- read_excel("./data/2017 sfr model data/aqueduct-30-country-rankings.xlsx", 
                      sheet = "results country")
dat_clean <- dat_raw %>%
  dplyr::filter(indicator_name == "bws",
         weight == "Tot") %>%
  dplyr::mutate(year = 2019,
         variablename = "Water stress",
         iso_a3 = country.code.name(name_0)) %>%
  dplyr::select(year, 
         iso3c = iso_a3, 
         value = score, 
         variablename) %>% 
  filter(value != -9999)

# NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

check_convert_iso(dat_clean, iso3c, iso3c_CONVERT,SCRIPT) # this is different from other scripts
check_coverage(dat_clean,"iso3c", "year", SCRIPT)
check_duplicates(dat_clean,"iso3c", "year", "variablename",SCRIPT)
check_nas(dat_clean, SCRIPT) # drop the iso3c convert before running this 

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

raw.data$water <- dat_clean
rmExcept("raw.data")