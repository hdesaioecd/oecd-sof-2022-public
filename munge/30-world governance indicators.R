source('./lib/funcs.R')
source("./lib/888-logging_unit-tests.R")

SCRIPT = "30-world-governance-indicators"

#import WGI
file_path <-
  "./data/2020 sfr model data/wgidataset_stata/wgidataset.dta"
library(haven)
dat_raw <- read_stata(file_path) %>%
  dplyr::select(
    iso3c = countryname,
    year,
    `Rule of law` = rle,
    `Regulatory quality` = rqe
  ) %>%
  tidyr::pivot_longer(names_to = "variablename",
                      values_to = "value",-c(iso3c, year))
dat_raw$iso3c <- country.code.name(dat_raw$iso3c)
dat_clean <- na.omit(dat_raw)

# MWQ
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

# Adding the checks
check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_clean, "iso3c", "year", SCRIPT)
check_duplicates(dat_clean, "iso3c", "year", "variablename", SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c, year, value, variablename)

check_nas(dat_clean, SCRIPT)

raw.data$wgi <- dat_clean
rmExcept('raw.data')