source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "29-EPI"

# https://epi.yale.edu/downloads
indicators <- setDT(raw.data$log)[source == "EPI"] %>%
  pull(variablename)
dat_raw <-
  fread("./data/2020 sfr model data/epi2022results05302022.csv")
setDT(dat_raw)
dat_raw$code <- NULL
dat_raw$iso <- NULL
dat_raw$region <- NULL
dat_clean <- dat_raw %>%
  tidyr::pivot_longer(names_to = "variablename",
                      values_to = "value",-country)
dat_final <-
  setDT(dat_clean)[variablename %in% indicators, .(iso3c = country,
                                                   year = 2021,
                                                   variablename,
                                                   value)]

dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)
dat_final$iso3c_CONVERT <-
  country.code.name(dat_final$iso3c_CONVERT)

# Adding the checks
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final, "iso3c", "year", SCRIPT)
check_duplicates(dat_final, "iso3c", "year", "variablename", SCRIPT)

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c, year, value, variablename)

check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this

raw.data$epi <- dat_final
rmExcept("raw.data")