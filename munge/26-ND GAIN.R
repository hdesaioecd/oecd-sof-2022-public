source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "26-ND-GAIN"

# ND GAIN: https://gain.nd.edu/our-work/country-index/
#adaptive capacity
file_path_cap <-
  "./data/2020 sfr model data/nd-gain/vulnerability/capacity.csv"
cap_raw <- fread(file_path_cap,header = T) 

cap_clean <- cap_raw %>%
  dplyr::select(-Name) %>%
  dplyr::rename(iso3c = ISO3) %>%
  tidyr::pivot_longer(names_to = "year",
                      values_to = "value", -iso3c) %>%
  dplyr::mutate(variablename = "Adaptive capacity",
         iso3c_CONVERT = country.code.name(iso3c)) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  tidyr::drop_na()

# Adding the checks
check_convert_iso(cap_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(cap_clean,"iso3c", "year", SCRIPT)
check_duplicates(cap_clean,"iso3c", "year", "variablename",SCRIPT)

cap_clean$iso3c = cap_clean$iso3c_CONVERT
cap_clean = cap_clean %>% select(iso3c,year,value,variablename)

check_nas(cap_clean, SCRIPT) # drop the iso3c convert before running this

raw.data$capacity <- cap_clean
rmExcept("raw.data")