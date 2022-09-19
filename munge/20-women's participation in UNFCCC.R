source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

# https://genderclimatetracker.org/womens-participation-party-delegations

SCRIPT = "20-womens-participation-UNFCCC"

dat_raw <-
  read_excel("./data/2020 sfr model data/Percentage of Women by Party.xlsx",
             skip = 1) %>%
  dplyr::select(-...3) %>%
  dplyr::rename(year = ...1,
                event = ...2) %>%
  tidyr::fill(year,
              event)

#take the value for COP (most important conference each year)
dat_clean <- dat_raw %>% 
  dplyr::filter(grepl("COP",event)) %>%
  dplyr::select(-event) %>%
  tidyr::pivot_longer(names_to = "country",
                      values_to = "value", -c(year)) %>%
  tidyr::drop_na(value)

dat_clean$iso3c <- country.code.name(dat_clean$country)
dat_clean$variablename <-
  "Women's participation in UNFCCC delegations"

dat_final <- na.omit(dat_clean)
dat_final$country <- NULL

dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)

check_convert_iso(dat_final,iso3c,iso3c_CONVERT, SCRIPT) # this is different from other scripts
check_coverage(dat_final,"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", "variablename",SCRIPT)
check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this 

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c,year,value,variablename)

raw.data$unfccc <- dat_final
rmExcept("raw.data")