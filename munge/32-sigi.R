source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "32-SIGI"

# https://www.genderindex.org/data/
dat_raw <-
  fread("./data/2020 sfr model data/GIDDB2019_22042022145601645.csv") %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names()

dat_clean <- dat_raw[variable == "Attitudes" &
                  region_2 == "All regions" &
                  income == "All income groups"]

dat_clean$variablename = "SIGI"

# NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$country)

# Adding the checks
check_convert_iso(dat_clean, country, iso3c_CONVERT, SCRIPT)
check_coverage(dat_clean,"country", "year", SCRIPT)
check_duplicates(dat_clean,"country", "year", "variablename",SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

check_nas(dat_clean, SCRIPT) 

dat_final = dat_clean
dat_final$iso3c <- country.code.name(dat_clean$iso3c)
raw.data$sigi <- dat_final
rmExcept("raw.data")
