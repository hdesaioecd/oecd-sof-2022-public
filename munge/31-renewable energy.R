source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "31-renewable-energy"

# USING OurWorldInData dataset: https://ourworldindata.org/grapher/share-electricity-renewables
dat_raw <- fread('./data/2020 sfr model data/share-electricity-renewables.csv') %>%
  janitor::clean_names()
dat_clean <- setDT(dat_raw)[,.(iso3c = country.code.name(entity),
                        year,
                        value = renewables_percent_electricity,
                        variablename = "Renewable energy share")]
dat_final <- na.omit(dat_clean)

dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)

# Adding the checks
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final,"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", "variablename",SCRIPT)

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c,year,value,variablename)

check_nas(dat_final, SCRIPT) 

raw.data$energy <- dat_final
rmExcept("raw.data")